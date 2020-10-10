use crate::memory_map::Interconnect;

use super::widget::Widget;
use std::borrow::Cow;
use tui::{
    backend::CrosstermBackend,
    layout::{Alignment, Rect},
    widgets::{Block, Borders, Paragraph, Text},
    Frame,
};

/// A widget showing the current status of the emulator (running, ...)
pub struct Status {
    status: &'static str,
}

impl Status {
    pub fn new() -> Self {
        Self { status: "Idle" }
    }

    pub fn set_status(&mut self, status: &'static str) {
        self.status = status;
    }
}

impl Widget for Status {
    fn refresh(&mut self, _: &crate::cpu::CPU, _: &Interconnect) {}

    fn draw(
        &mut self,
        f: &mut Frame<CrosstermBackend<std::io::Stdout>>,
        chunk: Rect,
        _: &crate::cpu::CPU,
        _: &Interconnect,
    ) {
        let text = &[Text::Raw(Cow::Borrowed(self.status))];
        let paragraph = Paragraph::new(text.iter())
            .block(Block::default().borders(Borders::NONE))
            .alignment(Alignment::Right);

        f.render_widget(paragraph, chunk);
    }

    fn select(&mut self) {}

    fn deselect(&mut self) {}

    fn is_selected(&self) -> bool {
        false
    }
}
