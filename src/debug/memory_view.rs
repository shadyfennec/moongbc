use super::widget::WidgetKind;
use crate::{
    cpu::CPU,
    debug::{util::FromHexString, widget::Widget},
    gpu::OAM_START,
    gpu::VRAM_START,
    memory_map::Mem,
    memory_map::ECHO_START,
    memory_map::ERAM_START,
    memory_map::HRAM_START,
    memory_map::IO_START,
    memory_map::ROM_START,
    memory_map::WRAM_START,
    Dst, Src,
};
use crossterm::event::{KeyCode, KeyEvent};
use std::{borrow::Cow, io::Stdout};
use tui::{
    backend::CrosstermBackend,
    layout::{Alignment, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Text},
    Frame,
};

enum Command {
    Goto,
    Set,
}

/// Represents the widget responsible for the memory map, able to navigate through the address
/// space and display the content of the memory.
pub struct MemoryView<'a> {
    text: Vec<Text<'a>>,
    start: u16,
    pos: u16,
    init: bool,
    height: usize,
    selected: bool,
    title_style: Style,
    command: Option<Command>,
}

impl<'a> Widget for MemoryView<'a> {
    fn refresh(&mut self, cpu: &CPU) {
        let location = cpu.memory_map.location(self.pos);
        let top = (0u8..0x10)
            .map(|i| format!(" {:02x}", i))
            .collect::<String>();

        let top = Text::Raw(Cow::Owned(format!("{} │{}\n", location, top)));
        let line = Text::Raw(Cow::Borrowed(
            "─────┼────────────────────────────────────────────────\n",
        ));

        let mut text = vec![top, line];

        let mut lines = (0u16..(self.height as u16).saturating_sub(2))
            .filter_map(|h| {
                if let Some(addr) = self.start.checked_add(0x10 * h) {
                    if addr < 0xFFFF {
                        let values = (0u16..0x10)
                            .map(|a| {
                                let addr = addr.saturating_add(a);
                                let end = if a == 0xF { "\n" } else { "" };
                                let data = if let Ok(data) = Mem(addr).try_read(cpu) {
                                    format!("{:02x}", data)
                                } else {
                                    String::from("??")
                                };
                                if addr == self.pos {
                                    Text::Styled(
                                        Cow::Owned(format!(" {}{}", data, end)),
                                        Style::default().fg(Color::Black).bg(Color::White),
                                    )
                                } else {
                                    Text::Raw(Cow::Owned(format!(" {}{}", data, end)))
                                }
                            })
                            .collect::<Vec<Text>>();

                        Some(
                            std::iter::once(Text::Raw(Cow::Owned(format!("{:04x} │", addr))))
                                .chain(values.into_iter()),
                        )
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .flatten()
            .collect::<Vec<Text>>();

        text.append(&mut lines);
        self.text = text;
    }

    fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, chunk: Rect, cpu: &CPU) {
        self.height = chunk.height as usize;
        if !self.init {
            self.init = true;
            self.refresh(cpu);
        }

        let paragraph = Paragraph::new(self.text.iter())
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Memory")
                    .title_style(self.title_style),
            )
            .alignment(Alignment::Center);

        f.render_widget(paragraph, chunk);
    }

    fn select(&mut self) {
        self.selected = true;
        self.title_style = Style::default().fg(Color::Yellow);
    }

    fn deselect(&mut self) {
        self.selected = false;
        self.title_style = Style::default();
    }

    fn is_selected(&self) -> bool {
        self.selected
    }

    fn handle_key(&mut self, key: KeyEvent, _: &mut CPU) -> Option<(WidgetKind, String)> {
        match key.code {
            KeyCode::Char('g') => {
                self.command = Some(Command::Goto);
                Some((WidgetKind::Memory, String::from("Go to address:")))
            }
            KeyCode::Char('s') => {
                self.command = Some(Command::Set);
                Some((
                    WidgetKind::Memory,
                    format!("Set memory at x{:04x}", self.pos),
                ))
            }
            KeyCode::Up => {
                self.pos = self.pos.checked_sub(0x10).unwrap_or(self.pos);
                if self.pos < self.start {
                    self.start = self.start.saturating_sub(0x10);
                }
                None
            }
            KeyCode::Down => {
                self.pos = self.pos.checked_add(0x10).unwrap_or(self.pos);
                if self.pos < 0xFFFF
                    && self.pos >= self.start.saturating_add(0x10 * (self.height as u16 - 4))
                {
                    self.start = self.start.saturating_add(0x10);
                }
                None
            }
            KeyCode::Left => {
                self.pos = self.pos.saturating_sub(1);
                if self.pos < self.start {
                    self.start = self.start.saturating_sub(0x10);
                }
                None
            }
            KeyCode::Right => {
                self.pos = self.pos.saturating_add(1);
                if self.pos < 0xFFFF
                    && self.pos >= self.start.saturating_add(0x10 * (self.height as u16 - 4))
                {
                    self.start = self.start.saturating_add(0x10);
                }
                None
            }
            _ => None,
        }
    }

    fn process_input(&mut self, input: String, cpu: &mut CPU) -> Result<(), Option<String>> {
        match &self.command {
            Some(c) => match c {
                Command::Goto => match input.to_lowercase().as_str() {
                    "boot" | "rom" => {
                        self.goto(ROM_START);
                        Ok(())
                    }
                    "eram" | "external ram" => {
                        self.goto(ERAM_START);
                        Ok(())
                    }
                    "vram" | "video ram" => {
                        self.goto(VRAM_START);
                        Ok(())
                    }
                    "wram" | "work ram" | "ram" | "working ram" => {
                        self.goto(WRAM_START);
                        Ok(())
                    }
                    "echo" | "echo ram" => {
                        self.goto(ECHO_START);
                        Ok(())
                    }
                    "oam" | "object attribute memory" | "object attributes memory" => {
                        self.goto(OAM_START);
                        Ok(())
                    }
                    "io" | "io registers" | "i/o registers" | "i/o" => {
                        self.goto(IO_START);
                        Ok(())
                    }
                    "hram" | "high ram" => {
                        self.goto(HRAM_START);
                        Ok(())
                    }
                    _ => match u16::from_hex_string(input) {
                        Ok(addr) => {
                            self.goto(addr);
                            Ok(())
                        }
                        Err(_) => Err(None),
                    },
                },
                Command::Set => match u8::from_hex_string(input) {
                    Ok(value) => Mem(self.pos)
                        .try_write(cpu, value)
                        .map_err(|e| Some(format!("{}", e))),
                    Err(_) => Err(None),
                },
            },
            None => Ok(()),
        }
    }
}

impl<'a> MemoryView<'a> {
    /// Creates a new `MemoryView` widget.
    pub fn new() -> MemoryView<'a> {
        MemoryView {
            text: vec![],
            start: 0,
            pos: 0,
            init: false,
            height: 0,
            selected: false,
            title_style: Style::default(),
            command: None,
        }
    }

    fn goto(&mut self, addr: u16) {
        let start = addr & 0xFFF0;
        self.start = start;
        self.pos = addr;
    }
}
