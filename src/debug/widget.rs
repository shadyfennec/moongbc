use super::input::Input;
use crate::cpu::CPU;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use std::io::Stdout;
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    Frame,
};

pub trait Widget {
    fn refresh(&mut self, cpu: &CPU);
    fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, chunk: Rect, cpu: &CPU);
    fn select(&mut self);
    fn deselect(&mut self);
    fn is_selected(&self) -> bool;
    fn handle_key(&mut self, _: KeyEvent, _: &mut CPU) -> Option<(WidgetKind, String)> {
        None
    }
    fn process_input(&mut self, _: String, _: &mut CPU) -> Result<(), Option<String>> {
        Ok(())
    }

    fn title_style(&self) -> Style {
        if self.is_selected() {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default()
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum WidgetKind {
    Assembly,
    Breakpoints,
    Memory,
    Registers,
}

impl WidgetKind {
    pub fn left(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => Some(WidgetKind::Registers),
            WidgetKind::Breakpoints => Some(WidgetKind::Memory),
            WidgetKind::Memory => None,
            WidgetKind::Registers => None,
        }
    }

    pub fn right(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => None,
            WidgetKind::Breakpoints => None,
            WidgetKind::Memory => Some(WidgetKind::Breakpoints),
            WidgetKind::Registers => Some(WidgetKind::Assembly),
        }
    }

    pub fn up(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => None,
            WidgetKind::Breakpoints => Some(WidgetKind::Assembly),
            WidgetKind::Memory => Some(WidgetKind::Registers),
            WidgetKind::Registers => None,
        }
    }

    pub fn down(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => Some(WidgetKind::Breakpoints),
            WidgetKind::Breakpoints => None,
            WidgetKind::Memory => None,
            WidgetKind::Registers => Some(WidgetKind::Memory),
        }
    }
}

type WidgetWithKind = (WidgetKind, Box<dyn Widget>);

pub struct WidgetList {
    widgets: Vec<WidgetWithKind>,
    input: Input,
    widget_callback: Option<WidgetKind>,
}

impl WidgetList {
    pub fn new() -> WidgetList {
        WidgetList {
            widgets: vec![],
            input: Input::new(),
            widget_callback: None,
        }
    }

    pub fn add(&mut self, widget: Box<dyn Widget>, kind: WidgetKind) {
        self.widgets.push((kind, widget));
    }

    pub fn refresh(&mut self, cpu: &CPU) {
        self.widgets.iter_mut().for_each(|(_, w)| {
            w.refresh(cpu);
        })
    }

    pub fn select(&mut self, kind: Option<WidgetKind>) {
        self.widgets.iter_mut().for_each(|(k, w)| match (&kind, k) {
            (Some(a), b) if *a == *b => w.select(),
            _ => w.deselect(),
        });
    }

    pub fn selected(&mut self) -> Option<usize> {
        self.widgets.iter().position(|(_, w)| w.is_selected())
    }

    pub fn find(&self, kind: &WidgetKind) -> Option<usize> {
        self.widgets.iter().position(|(k, _)| *k == *kind)
    }

    fn event_to_key(&self, event: Event) -> KeyEvent {
        match event {
            Event::Key(ke) => ke,
            _ => panic!("Not a key event"),
        }
    }

    fn handle_input(&mut self, event: Event, cpu: &mut CPU) -> Option<()> {
        if self.input.is_selected() {
            let key = self.event_to_key(event);
            if let Some(s) = self.input.handle_input_key(key) {
                match s {
                    Ok(s) => {
                        if let Some(k) = self.widget_callback {
                            match self
                                .find(&k)
                                .map(|i| self.widgets[i].1.process_input(s, cpu))
                                .unwrap_or_else(|| Err(Some(r#"Widget not found"#.to_string())))
                            {
                                Ok(_) => {
                                    self.select(Some(k));
                                    self.input.deselect();
                                    self.input.set_title(String::new());
                                    if let Some(i) = self.find(&k) {
                                        self.widgets[i].1.refresh(cpu)
                                    }
                                }
                                Err(e) => {
                                    self.input.set_error(true);
                                    self.input.set_error_title(e);
                                }
                            }
                        }
                    }
                    Err(_) => {
                        if let Some(k) = self.widget_callback {
                            self.select(Some(k));
                            self.input.deselect();
                            self.input.set_title(String::new());
                            if let Some(i) = self.find(&k) {
                                self.widgets[i].1.refresh(cpu)
                            }
                        }
                    }
                }
            }
            Some(())
        } else {
            None
        }
    }

    fn global_keys(&mut self, event: Event, ret: &mut bool, cpu: &mut CPU) -> Option<()> {
        let key = self.event_to_key(event);
        match key.code {
            KeyCode::Char('q') => {
                *ret = true;
                Some(())
            }
            KeyCode::Char('n') => {
                cpu.step();
                self.refresh(cpu);
                Some(())
            }
            KeyCode::Char('r') => loop {
                cpu.step();
                self.refresh(cpu);
                if cpu.check_breakpoints() {
                    break Some(());
                }
            },
            KeyCode::Esc => {
                self.select(None);
                Some(())
            }
            _ => None,
        }
    }

    fn arrow_keys(&mut self, event: Event, _: &mut CPU) -> Option<()> {
        if let Some(kind) = self.selected().map(|i| self.widgets[i].0) {
            match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Left,
                    modifiers: KeyModifiers::SHIFT,
                }) => {
                    self.select(Some(kind.left().unwrap_or(kind)));
                    Some(())
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Right,
                    modifiers: KeyModifiers::SHIFT,
                }) => {
                    self.select(Some(kind.right().unwrap_or(kind)));
                    Some(())
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Up,
                    modifiers: KeyModifiers::SHIFT,
                }) => {
                    self.select(Some(kind.up().unwrap_or(kind)));
                    Some(())
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Down,
                    modifiers: KeyModifiers::SHIFT,
                }) => {
                    self.select(Some(kind.down().unwrap_or(kind)));
                    Some(())
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn direct_widget_keys(&mut self, event: Event, _: &mut CPU) -> Option<()> {
        let key = self.event_to_key(event);
        match key.code {
            KeyCode::Char('D') => {
                self.select(Some(WidgetKind::Assembly));
                Some(())
            }
            KeyCode::Char('R') => {
                self.select(Some(WidgetKind::Registers));
                Some(())
            }
            KeyCode::Char('M') => {
                self.select(Some(WidgetKind::Memory));
                Some(())
            }
            KeyCode::Char('B') => {
                self.select(Some(WidgetKind::Breakpoints));
                Some(())
            }
            _ => None,
        }
    }

    pub fn handle_key(&mut self, key: Event, cpu: &mut CPU) -> bool {
        let mut ret = false;
        self.handle_input(key, cpu)
            .or_else(|| self.global_keys(key, &mut ret, cpu))
            .or_else(|| self.arrow_keys(key, cpu))
            .or_else(|| self.direct_widget_keys(key, cpu))
            .or_else(|| {
                let key = self.event_to_key(key);
                if let Some((kind, string)) = {
                    self.selected()
                        .map(|i| self.widgets[i].1.handle_key(key, cpu))
                        .unwrap_or(None)
                } {
                    self.widget_callback = Some(kind);
                    self.input.set_title(string);
                    self.input.select();
                } else {
                    self.widget_callback = None;
                }
                Some(())
            });
        self.refresh(cpu);

        ret
    }

    pub fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, cpu: &CPU) {
        let top_bottom = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Length(f.size().height.saturating_sub(3)),
                    Constraint::Min(3),
                ]
                .as_ref(),
            )
            .split(f.size());

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
            .split(top_bottom[0]);

        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(70), Constraint::Percentage(30)].as_ref())
            .split(chunks[1]);

        let left_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(5), Constraint::Percentage(100)])
            .split(chunks[0]);

        vec![
            (&WidgetKind::Registers, left_chunks[0]),
            (&WidgetKind::Memory, left_chunks[1]),
            (&WidgetKind::Assembly, right_chunks[0]),
            (&WidgetKind::Breakpoints, right_chunks[1]),
        ]
        .into_iter()
        .for_each(|(k, r)| {
            self.find(k)
                .map(|i| self.widgets[i].1.as_mut())
                .unwrap()
                .draw(f, r, cpu);
        });

        self.input.draw(f, top_bottom[1], cpu);
    }
}
