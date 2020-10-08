use super::widget::WidgetKind;
use crate::{
    cpu::{Breakpoint, InterruptKind, WatchKind, Watcher, CPU},
    debug::{util::FromHexString, widget::Widget},
    memory_map::Mem,
    register::{Flag, Reg16, Reg8},
};
use crossterm::event::{KeyCode, KeyEvent};
use std::{borrow::Cow, io::Stdout};
use tui::{
    backend::CrosstermBackend,
    layout::Rect,
    style::{Color, Style},
    widgets::{Block, Borders, List, ListState, Text},
    Frame,
};

fn parse_reg(input: String, cpu: &mut CPU) -> Result<(), Option<String>> {
    match input.as_str() {
        "af" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::AF,
                    cpu,
                ))));
            Ok(())
        }
        "bc" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::BC,
                    cpu,
                ))));
            Ok(())
        }
        "de" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::DE,
                    cpu,
                ))));
            Ok(())
        }
        "hl" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::HL,
                    cpu,
                ))));
            Ok(())
        }
        "sp" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::SP,
                    cpu,
                ))));
            Ok(())
        }
        "pc" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg16(Watcher::new(
                    Reg16::PC,
                    cpu,
                ))));
            Ok(())
        }
        "a" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::A,
                    cpu,
                ))));
            Ok(())
        }
        "f" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::F,
                    cpu,
                ))));
            Ok(())
        }
        "b" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::B,
                    cpu,
                ))));
            Ok(())
        }
        "c" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::C,
                    cpu,
                ))));
            Ok(())
        }
        "d" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::D,
                    cpu,
                ))));
            Ok(())
        }
        "e" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::E,
                    cpu,
                ))));
            Ok(())
        }
        "h" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::H,
                    cpu,
                ))));
            Ok(())
        }
        "l" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Reg8(Watcher::new(
                    Reg8::L,
                    cpu,
                ))));
            Ok(())
        }
        _ => Err(None),
    }
}

fn parse_memory(input: String, cpu: &mut CPU) -> Result<(), Option<String>> {
    match u16::from_hex_string(input) {
        Ok(addr) => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Mem(Watcher::new(
                    Mem(addr),
                    cpu,
                ))));
            Ok(())
        }
        Err(_) => Err(None),
    }
}

fn parse_flag(input: String, cpu: &mut CPU) -> Result<(), Option<String>> {
    match input.as_str() {
        "z" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Flag(Watcher::new(
                    Flag::Z,
                    cpu,
                ))));
            Ok(())
        }
        "n" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Flag(Watcher::new(
                    Flag::Z,
                    cpu,
                ))));
            Ok(())
        }
        "h" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Flag(Watcher::new(
                    Flag::Z,
                    cpu,
                ))));
            Ok(())
        }
        "c" => {
            cpu.breakpoints
                .push(Breakpoint::Watch(WatchKind::Flag(Watcher::new(
                    Flag::Z,
                    cpu,
                ))));
            Ok(())
        }
        _ => Err(None),
    }
}

type ParseFunction = dyn Fn(String, &mut CPU) -> Result<(), Option<String>>;

enum Command {
    Address,
    Opcode,
    Watch,
    Interrupt,
}

/// Represents the widget responsible for displaying the different
/// breakpoints created during the debugging session.
pub struct BreakpointView {
    init: bool,
    selected: bool,
    cursor: Option<usize>,
    command: Option<Command>,
    breakpoint_count: usize,
}

impl BreakpointView {
    /// Creates a new `BreakpointView` widget.
    pub fn new() -> Self {
        BreakpointView {
            init: false,
            selected: false,
            cursor: None,
            command: None,
            breakpoint_count: 0,
        }
    }
}

impl Widget for BreakpointView {
    fn refresh(&mut self, cpu: &CPU) {
        if self.cursor.is_none() && !cpu.breakpoints.is_empty() {
            self.cursor = Some(0);
        }
        self.breakpoint_count = cpu.breakpoints.len();
    }

    fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, chunk: Rect, cpu: &CPU) {
        if !self.init {
            self.init = true;
            self.refresh(cpu);
        }

        let items = cpu
            .breakpoints
            .iter()
            .map(|b| Text::Raw(Cow::Owned(b.to_string(cpu))));

        let mut list_state = ListState::default();
        list_state.select(self.cursor);

        let list = List::new(items)
            .block(
                Block::default()
                    .title("Breakpoints")
                    .title_style(self.title_style())
                    .borders(Borders::ALL),
            )
            .style(Style::default())
            .highlight_style(Style::default().bg(Color::White).fg(Color::Black))
            .highlight_symbol("> ");

        f.render_stateful_widget(list, chunk, &mut list_state);
    }

    fn select(&mut self) {
        self.selected = true;
    }

    fn deselect(&mut self) {
        self.selected = false;
    }

    fn is_selected(&self) -> bool {
        self.selected
    }

    fn handle_key(&mut self, key: KeyEvent, cpu: &mut CPU) -> Option<(WidgetKind, String)> {
        match key.code {
            KeyCode::Char('b') => {
                self.command = Some(Command::Address);
                Some((WidgetKind::Breakpoints, "Break at address:".to_string()))
            }
            KeyCode::Char('o') => {
                self.command = Some(Command::Opcode);
                Some((WidgetKind::Breakpoints, "Break at opcode:".to_string()))
            }
            KeyCode::Char('w') => {
                self.command = Some(Command::Watch);
                Some((WidgetKind::Breakpoints, "Watch:".to_string()))
            }
            KeyCode::Char('i') => {
                self.command = Some(Command::Interrupt);
                Some((WidgetKind::Breakpoints, "Watch interrupt:".to_string()))
            }
            KeyCode::Char('d') => {
                if let Some(idx) = &self.cursor {
                    cpu.breakpoints.remove(*idx);
                    let len = cpu.breakpoints.len();
                    self.cursor = if len == 0 {
                        None
                    } else {
                        Some((*idx).min(len.saturating_sub(1)))
                    }
                }
                None
            }
            KeyCode::Down => {
                if let Some(idx) = &self.cursor {
                    self.cursor = Some(idx.saturating_add(1).min(self.breakpoint_count - 1));
                }
                None
            }
            KeyCode::Up => {
                if let Some(idx) = &self.cursor {
                    self.cursor = Some(idx.saturating_sub(1));
                }
                None
            }
            _ => None,
        }
    }

    fn process_input(&mut self, input: String, cpu: &mut CPU) -> Result<(), Option<String>> {
        if let Some(command) = &self.command {
            match command {
                Command::Address => match u16::from_hex_string(input) {
                    Ok(addr) => {
                        cpu.breakpoints.push(Breakpoint::Address(addr));
                        Ok(())
                    }
                    Err(_) => Err(None),
                },
                Command::Opcode => match u8::from_hex_string(input) {
                    Ok(op) => {
                        cpu.breakpoints.push(Breakpoint::Opcode(op));
                        Ok(())
                    }
                    Err(_) => Err(None),
                },
                Command::Watch => {
                    let kinds: Vec<(&str, &ParseFunction)> = vec![
                        ("memory", &parse_memory),
                        ("register", &parse_reg),
                        ("flag", &parse_flag),
                    ];
                    kinds
                        .into_iter()
                        .filter_map(|(s, f)| {
                            let lowercase = input.to_lowercase();
                            let tokens = lowercase.split(' ').take(2).collect::<Vec<&str>>();
                            if tokens.len() == 2 {
                                let (kind, input) = (tokens[0], tokens[1]);
                                if s.starts_with(kind.to_lowercase().as_str()) {
                                    Some((f)(input.to_string(), cpu))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .next()
                        .unwrap_or(Err(None))
                }
                Command::Interrupt => {
                    let kinds: Vec<(&'static str, InterruptKind)> = vec![
                        ("vblank", InterruptKind::VBlank),
                        ("lcd", InterruptKind::LCD),
                        ("timer", InterruptKind::Timer),
                        ("serial", InterruptKind::SerialIO),
                        ("joypad", InterruptKind::Joypad),
                    ];

                    kinds
                        .into_iter()
                        .filter_map(|(k, i)| {
                            let input = input.to_lowercase();
                            if k.starts_with(input.as_str()) {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .next()
                        .map(|i| {
                            cpu.add_breakpoint(Breakpoint::Interrupt(i));
                            Ok(())
                        })
                        .unwrap_or(Err(None))
                }
            }
        } else {
            Ok(())
        }
    }
}
