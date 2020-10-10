use super::widget::WidgetKind;
use crate::{
    cpu::CPU,
    debug::{util::FromHexString, widget::Widget},
    disassembly::{Disassembler, InstructionWindow},
    memory_map::{Interconnect, Mem, Mem16, MemIdx},
    opcode::{Decoder, Opcode},
    register::Reg16,
};
use crossterm::event::{KeyCode, KeyEvent};
use std::{borrow::Cow, io::Stdout};
use tui::{
    backend::CrosstermBackend,
    layout::Rect,
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Text},
    Frame,
};

enum Command {
    Goto,
}

/// The widget responsible for the disassembly output.
pub struct Assembly {
    instruction_window: Option<InstructionWindow>,
    init: bool,
    height: usize,
    selected: bool,
    pos: u16,
    old_pc: u16,
    sizes: [usize; 2],
    command: Option<Command>,
}

impl Widget for Assembly {
    fn refresh(&mut self, cpu: &CPU, memory: &Interconnect) {
        let pc = Reg16::PC.read(&cpu.registers);
        if self.old_pc != pc {
            self.old_pc = pc;
            self.pos = pc;
        }

        let (before, after) = Disassembler::new().surrounding_sizes(cpu, memory, self.pos);
        self.sizes[0] = before;
        self.sizes[1] = after;

        let instruction_window = InstructionWindow::build(
            self.height / 2,
            self.height - self.height / 2,
            self.pos,
            cpu,
            memory,
        );

        self.instruction_window = Some(instruction_window);
    }

    fn draw(
        &mut self,
        f: &mut Frame<CrosstermBackend<Stdout>>,
        chunk: Rect,
        cpu: &CPU,
        memory: &Interconnect,
    ) {
        self.height = chunk.height as usize;

        if !self.init {
            self.init = true;
            self.refresh(cpu, memory);
        }

        if let Some(instruction_window) = &self.instruction_window {
            let items = instruction_window
                .instructions
                .iter()
                .enumerate()
                .map(|(i, (a, o))| {
                    let is_cursor = instruction_window.cursor == i;
                    let is_pc = instruction_window.pc.map(|a| a == i).unwrap_or(false);

                    let (cursor, bg, fg) = match (is_cursor, is_pc) {
                        (false, false) => (' ', None, Color::White),
                        (false, true) => ('>', None, Color::Red),
                        (true, false) => (' ', Some(Color::White), Color::Black),
                        (true, true) => ('>', Some(Color::White), Color::Red),
                    };

                    let style = if let Some(bg) = bg {
                        Style::default().fg(fg).bg(bg)
                    } else {
                        Style::default().fg(fg)
                    };

                    let instr = match o.try_display(&cpu.registers, memory, Some(*a)) {
                        Ok(s) => s,
                        Err(s) => s,
                    };

                    Text::Styled(
                        Cow::Owned(format!("{} 0x{:04x} {}\n", cursor, *a, instr)),
                        style,
                    )
                })
                .collect::<Vec<_>>();

            let paragraph = Paragraph::new(items.iter()).block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Disassembly")
                    .title_style(self.title_style()),
            );

            f.render_widget(paragraph, chunk);
        }
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

    fn handle_key(
        &mut self,
        key: KeyEvent,
        cpu: &mut CPU,
        memory: &Interconnect,
    ) -> Option<(WidgetKind, String)> {
        match key.code {
            KeyCode::Down => {
                self.pos = self.pos.saturating_add(self.sizes[1] as u16);
                None
            }
            KeyCode::Up => {
                self.pos = self.pos.saturating_sub(self.sizes[0] as u16);
                None
            }
            KeyCode::Char('g') => {
                self.command = Some(Command::Goto);
                Some((WidgetKind::Assembly, String::from("Go to instruction:")))
            }
            KeyCode::Char('j') => {
                let opcode = Decoder::decode_with_context(&cpu.registers, memory, self.pos);

                if let Some(opcode) = opcode {
                    match opcode {
                        Opcode::CALL(_, _) => {
                            self.pos =
                                Mem16(MemIdx::Direct(self.pos + 1)).read(memory, &cpu.registers);
                        }
                        Opcode::JP(_, _) => {
                            self.pos =
                                Mem16(MemIdx::Direct(self.pos + 1)).read(memory, &cpu.registers);
                        }
                        Opcode::JPHL => {
                            self.pos = Reg16::HL.read(&cpu.registers);
                        }
                        Opcode::JR(_) => {
                            let offset = Mem(MemIdx::Direct(self.pos + 1))
                                .read(memory, &cpu.registers)
                                as i8;
                            self.pos = if offset < 0 {
                                self.pos.saturating_sub(((-offset) - 1) as u16)
                            } else {
                                self.pos.saturating_add((offset + 1) as u16)
                            };
                            println!("0x{:02x}", self.pos);
                        }
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn process_input(
        &mut self,
        input: String,
        cpu: &mut CPU,
        memory: &mut Interconnect,
    ) -> Result<(), Option<String>> {
        if let Some(command) = &self.command {
            match command {
                Command::Goto => match u16::from_hex_string(input) {
                    Ok(addr) => {
                        if Disassembler::new().check_address(cpu, memory, addr) {
                            self.pos = addr;
                            Ok(())
                        } else {
                            Err(Some(format!("Invalid instruction address: 0x{:04x}", addr)))
                        }
                    }
                    Err(_) => Err(None),
                },
            }
        } else {
            Ok(())
        }
    }
}

impl Assembly {
    /// Returns a new Assembly widget.
    pub fn new() -> Assembly {
        Assembly {
            instruction_window: None,
            init: false,
            height: 0,
            selected: false,
            pos: 0,
            old_pc: 0,
            sizes: [0, 0],
            command: None,
        }
    }
}
