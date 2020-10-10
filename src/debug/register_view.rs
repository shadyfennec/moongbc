use crate::{
    cpu::CPU,
    debug::widget::Widget,
    memory_map::Interconnect,
    register::{Flag, Reg16, Registers},
};
use std::{borrow::Cow, io::Stdout};
use tui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Text},
    Frame,
};

/// Represents the widget responsible for displaying the current values of
/// the different registers of the CPU.
pub struct RegisterView<'a> {
    registers: Registers,
    text: Vec<Text<'a>>,
    flags: Vec<Text<'a>>,
    init: bool,
    selected: bool,
    title_style: Style,
}

impl<'a> Widget for RegisterView<'a> {
    fn refresh(&mut self, cpu: &CPU, _: &Interconnect) {
        let space = Text::Raw(Cow::Borrowed("  "));

        let registers = vec![
            (Reg16::BC, false),
            (Reg16::AF, true),
            (Reg16::DE, false),
            (Reg16::SP, true),
            (Reg16::HL, false),
        ];

        let text = registers
            .into_iter()
            .flat_map(|(r, l)| {
                if l {
                    vec![self.get_register(cpu, r, l)]
                } else {
                    vec![self.get_register(cpu, r, l), space.clone()]
                }
            })
            .collect::<Vec<_>>();

        let pc = Reg16::PC.read(&cpu.registers);
        let pc = Text::Raw(Cow::Owned(format!("PC: 0x{:04x}\n", pc)));

        self.text = text
            .into_iter()
            .chain(std::iter::once(pc))
            .collect::<Vec<_>>();

        let flags = vec![
            (Flag::Z, false),
            (Flag::N, true),
            (Flag::H, false),
            (Flag::C, true),
        ];

        let flags = flags
            .into_iter()
            .flat_map(|(f, l)| {
                if l {
                    vec![self.get_flag(cpu, f, l)]
                } else {
                    vec![self.get_flag(cpu, f, l), space.clone()]
                }
            })
            .collect::<Vec<_>>();

        self.flags = flags;
    }

    fn draw(
        &mut self,
        f: &mut Frame<CrosstermBackend<Stdout>>,
        chunk: Rect,
        cpu: &CPU,
        memory: &Interconnect,
    ) {
        if !self.init {
            self.init = true;
            self.refresh(cpu, memory);
        }

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
            .split(chunk);

        let paragraph_registers = Paragraph::new(self.text.iter())
            .block(
                Block::default()
                    .title("Registers")
                    .borders(Borders::ALL)
                    .title_style(self.title_style),
            )
            .alignment(Alignment::Center);

        let paragraph_flags = Paragraph::new(self.flags.iter())
            .block(Block::default().title("Flags").borders(Borders::ALL))
            .alignment(Alignment::Center);

        f.render_widget(paragraph_registers, chunks[0]);
        f.render_widget(paragraph_flags, chunks[1]);
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
}

impl<'a> RegisterView<'a> {
    /// Creates a new `RegisterView` widget.
    pub fn new() -> RegisterView<'a> {
        RegisterView {
            registers: Registers::new(),
            text: vec![],
            flags: vec![],
            init: false,
            selected: false,
            title_style: Style::default(),
        }
    }

    fn get_register(&mut self, cpu: &CPU, reg: Reg16, line_break: bool) -> Text<'a> {
        let value = reg.read(&cpu.registers);
        let line_break = if line_break { "\n" } else { "" };

        if value != self.registers.get_16(&reg) {
            self.registers.set_16(&reg, value);
            Text::Styled(
                Cow::Owned(format!("{}: 0x{:04x}{}", reg, value, line_break)),
                Style::default().bg(Color::White).fg(Color::Black),
            )
        } else {
            Text::Raw(Cow::Owned(format!(
                "{}: 0x{:04x}{}",
                reg, value, line_break
            )))
        }
    }

    fn get_flag(&mut self, cpu: &CPU, flag: Flag, line_break: bool) -> Text<'a> {
        let value = flag.read(&cpu.registers);
        let line_break = if line_break { "\n" } else { "" };

        if value != self.registers.get_flag(&flag) {
            self.registers.set_flag(&flag, value);
            Text::Styled(
                Cow::Owned(format!("{}: {}{}", flag, value as u8, line_break)),
                Style::default().bg(Color::White).fg(Color::Black),
            )
        } else {
            Text::Raw(Cow::Owned(format!(
                "{}: {}{}",
                flag, value as u8, line_break
            )))
        }
    }
}
