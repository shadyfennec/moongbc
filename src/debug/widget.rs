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

pub enum KeyAction {
    Nothing,
    Run,
    Step,
    Quit,
}

/// Represents an interactable element of the terminal UI, presenting data and possibly ways to
/// interact with them.
pub trait Widget {
    /// Refreshes the content of the widget, readying it to be displayed with up-to-date information.
    fn refresh(&mut self, cpu: &CPU);

    /// Draws the widget on the screen, in the area specified by `chunk`.
    fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, chunk: Rect, cpu: &CPU);

    /// Selects the widget, in order to toggle visual elements that shows that the widget
    /// is selected (title color, borders, ...)
    fn select(&mut self);

    /// Deselects the widget.
    fn deselect(&mut self);

    /// Returns whether or not the widget is currently selected
    fn is_selected(&self) -> bool;

    /// Handles a `KeyEvent` input from the user, processing it. The widget can either
    /// handle the input internally (for example, shifting a cursor upwards or downwards in a list),
    /// in which case it returns `None`; or it can signal that the input leads to a text input,
    /// in which case it returns `Some` tuple containing the `WidgetKind` to call back after
    /// the text input, and a `String` which represents the prompt.
    ///
    /// For example, if the widget returns `Some(Assembly, "Go to address:")`, this means
    /// that the widget requires a text input, that will be then processed by the widget
    /// with the kind `Assembly`, with a prompt reading `"Go to address:"`. Usually, the
    /// `WidgetKind` returned is the same as the one from the widget called.
    ///
    /// The text input is usually then processed by using the `process_input` function.
    fn handle_key(&mut self, _: KeyEvent, _: &mut CPU) -> Option<(WidgetKind, String)> {
        None
    }

    /// Processes a text input from the input widget. This is generally used after a call
    /// of `handle_key` that returned `Some`. The text input is usually processed according to
    /// the last command that was requested before the text input; this requires some kind of
    /// memorisation on the implementation side.
    ///
    /// The input can be malformed and not suitable for the widget, in which case the function
    /// returns `Err(Option<String>)`. The `Option` here is used to possibly send to the input
    /// widget a custom error message.
    ///
    /// If the input was processed successfully, the function returns `Ok(())`.
    fn process_input(&mut self, _: String, _: &mut CPU) -> Result<(), Option<String>> {
        Ok(())
    }

    /// Returns the `Style` used for the title, depending on if the widget is currently selected
    /// or not.
    fn title_style(&self) -> Style {
        if self.is_selected() {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default()
        }
    }
}

/// The different kind of widgets present in this application.
#[derive(PartialEq, Copy, Clone)]
pub enum WidgetKind {
    /// The disassembly window, implemented by the `Assembly` widget.
    Assembly,
    /// The breakpoint visualisation window, implemented by the `BreakpointView` widget.
    Breakpoints,
    /// The memory map visualisation window, implemented by the `MemoryView` widget.
    Memory,
    /// The register visualisation window, implemented by the `RegisterView` widget.
    Registers,
}

impl WidgetKind {
    /// Returns the widget on the left of this widget.
    pub fn left(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => Some(WidgetKind::Registers),
            WidgetKind::Breakpoints => Some(WidgetKind::Memory),
            WidgetKind::Memory => None,
            WidgetKind::Registers => None,
        }
    }

    /// Returns the widget on the right of this widget.
    pub fn right(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => None,
            WidgetKind::Breakpoints => None,
            WidgetKind::Memory => Some(WidgetKind::Breakpoints),
            WidgetKind::Registers => Some(WidgetKind::Assembly),
        }
    }

    /// Returns the widget on top of this widget.
    pub fn up(&self) -> Option<WidgetKind> {
        match self {
            WidgetKind::Assembly => None,
            WidgetKind::Breakpoints => Some(WidgetKind::Assembly),
            WidgetKind::Memory => Some(WidgetKind::Registers),
            WidgetKind::Registers => None,
        }
    }

    /// Returns the widget underneath this widget.
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

/// Represents the list of widgets in the application, responsible
/// of refreshing, drawing, and dispatching the inputs to the
/// correct widget.
pub struct WidgetList {
    widgets: Vec<WidgetWithKind>,
    input: Input,
    widget_callback: Option<WidgetKind>,
}

impl WidgetList {
    /// Creates a new, empty `WidgetList`.
    pub fn new() -> WidgetList {
        WidgetList {
            widgets: vec![],
            input: Input::new(),
            widget_callback: None,
        }
    }

    /// Adds a new widget to the list
    pub fn add(&mut self, widget: Box<dyn Widget>, kind: WidgetKind) {
        self.widgets.push((kind, widget));
    }

    /// Refreshes all the widgets in the list.
    pub fn refresh(&mut self, cpu: &CPU) {
        self.widgets.iter_mut().for_each(|(_, w)| {
            w.refresh(cpu);
        })
    }

    /// Selects a widget based on the specified kind, and deselects all the others.
    /// If `None` is passed, then everything is deselected.
    pub fn select(&mut self, kind: Option<WidgetKind>) {
        self.widgets.iter_mut().for_each(|(k, w)| match (&kind, k) {
            (Some(a), b) if *a == *b => w.select(),
            _ => w.deselect(),
        });
    }

    /// Returns the index of the widget that is currently selected.
    pub fn selected(&mut self) -> Option<usize> {
        self.widgets.iter().position(|(_, w)| w.is_selected())
    }

    /// Returns the index of the widget corresponding to the specified kind.
    pub fn find(&self, kind: &WidgetKind) -> Option<usize> {
        self.widgets.iter().position(|(k, _)| *k == *kind)
    }

    pub fn display_error<S: Into<String>>(&mut self, message: S) {
        self.input.set_error(true);
        self.input.set_error_title(Some(message.into()));
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

    fn global_keys(&mut self, event: Event, ret: &mut KeyAction) -> Option<()> {
        let key = self.event_to_key(event);
        match key.code {
            KeyCode::Char('q') => {
                *ret = KeyAction::Quit;
                Some(())
            }
            KeyCode::Char('n') => {
                *ret = KeyAction::Step;
                Some(())
            }
            KeyCode::Char('r') => {
                *ret = KeyAction::Run;
                Some(())
            }

            KeyCode::Char(' ') => {
                *ret = KeyAction::Nothing;
                Some(())
            }
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

    /// Handles a keyboard input from the terminal, and dispatches it to the correct widget, if any.
    /// This function returns `true` if the program should quit.
    pub fn handle_key(&mut self, key: Event, cpu: &mut CPU) -> KeyAction {
        let mut ret = KeyAction::Nothing;
        self.handle_input(key, cpu)
            .or_else(|| self.global_keys(key, &mut ret))
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
        ret
    }

    // This might be beneficial for the user to be able to configure this, but it might
    // just be too much trouble...
    /// Draws all the widgets contained in the list, according to a hard-coded layout.
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
