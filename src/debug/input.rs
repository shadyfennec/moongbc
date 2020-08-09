use crate::{cpu::CPU, debug::widget::Widget};
use crossterm::event::{KeyCode, KeyEvent};
use std::{borrow::Cow, io::Stdout};
use tui::{
    backend::CrosstermBackend,
    layout::{Alignment, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Text},
    Frame,
};

struct InputString {
    chars: Vec<char>,
    pos: usize,
}

impl InputString {
    pub fn new() -> InputString {
        InputString {
            chars: vec![],
            pos: 0,
        }
    }

    pub fn add(&mut self, c: char) {
        self.chars.insert(self.pos, c);
        self.pos += 1;
    }

    pub fn remove(&mut self) {
        if !self.chars.is_empty() && self.pos != 0 {
            self.chars.remove(self.pos.saturating_sub(1));
            self.pos = self.pos.saturating_sub(1);
        }
    }

    pub fn left(&mut self) {
        self.pos = self.pos.saturating_sub(1);
    }

    pub fn right(&mut self) {
        self.pos = std::cmp::min(self.pos + 1, self.chars.len())
    }

    pub fn beginning(&mut self) {
        self.pos = 0;
    }

    pub fn end(&mut self) {
        self.pos = self.chars.len();
    }

    pub fn clear(&mut self) {
        self.chars = vec![];
        self.pos = 0;
    }

    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        self.chars.iter().collect::<_>()
    }

    pub fn to_text(&self, selected: bool) -> Vec<Text> {
        self.chars
            .iter()
            .copied()
            .enumerate()
            .chain(std::iter::once((self.chars.len(), ' ')))
            .map(|(i, c)| {
                if selected && i == self.pos {
                    Text::Styled(
                        Cow::Owned(c.to_string()),
                        Style::default().bg(Color::White).fg(Color::Black),
                    )
                } else {
                    Text::Raw(Cow::Owned(c.to_string()))
                }
            })
            .collect::<Vec<_>>()
    }
}

/// A special widget, used to handle text input from the user.
pub struct Input {
    input: InputString,
    title: String,
    selected: bool,
    error: bool,
    error_title: Option<String>,
    title_style: Style,
}

impl Widget for Input {
    fn refresh(&mut self, _: &CPU) {}
    fn draw(&mut self, f: &mut Frame<CrosstermBackend<Stdout>>, chunk: Rect, _: &CPU) {
        let text = self.input.to_text(self.selected);

        let title = if self.error {
            format!(
                "{} {}",
                self.title,
                self.error_title.as_deref().unwrap_or("(invalid input)")
            )
        } else {
            self.title.to_string()
        };
        let paragraph = Paragraph::new(text.iter())
            .block(
                Block::default()
                    .title_style(self.title_style)
                    .title(title.as_str())
                    .borders(Borders::NONE),
            )
            .alignment(Alignment::Left);

        f.render_widget(paragraph, chunk);
    }
    fn select(&mut self) {
        self.selected = true;
        self.title_style = Style::default().fg(Color::Yellow);
        self.input.clear();
    }
    fn deselect(&mut self) {
        self.selected = false;
        self.title_style = Style::default();
        self.error = false;
        self.input.clear();
        self.error_title = None;
    }
    fn is_selected(&self) -> bool {
        self.selected
    }
}

impl Input {
    /// Creates a new `Input` widget.
    pub fn new() -> Input {
        Input {
            input: InputString::new(),
            title: String::new(),
            error: false,
            error_title: None,
            selected: false,
            title_style: Style::default(),
        }
    }

    /// Sets the title (prompt) of the input.
    pub fn set_title(&mut self, t: String) {
        self.title = t;
    }

    /// Sets the error status of the input.
    pub fn set_error(&mut self, error: bool) {
        self.error = error;
    }

    /// Sets the text to be displayed in the event of an error.
    /// If the specified title is `None`, a default text will appear instead.
    pub fn set_error_title(&mut self, error_title: Option<String>) {
        self.error_title = error_title;
    }

    /// Handles input keys and returns:
    /// - `Some(Ok(String))` if the input is complete and the text should be
    ///   forwarded to the correct widget,
    /// - `Some(Err(()))` if the input was not carried through (typically when
    ///   pressing the Esc key), and no input text should be forwarded to the widget,
    /// - `None` if the input is not finished, and remains to be typed.
    pub fn handle_input_key(&mut self, key: KeyEvent) -> Option<Result<String, ()>> {
        match key.code {
            KeyCode::Char(ch) => {
                self.input.add(ch);
                None
            }
            KeyCode::Backspace => {
                self.input.remove();
                None
            }
            KeyCode::Left => {
                self.input.left();
                None
            }
            KeyCode::Right => {
                self.input.right();
                None
            }
            KeyCode::Up => {
                self.input.end();
                None
            }
            KeyCode::Down => {
                self.input.beginning();
                None
            }
            KeyCode::Esc => Some(Err(())),
            KeyCode::Enter => {
                let result = Some(Ok(self.input.to_string()));
                self.input.clear();
                result
            }
            _ => None,
        }
    }
}
