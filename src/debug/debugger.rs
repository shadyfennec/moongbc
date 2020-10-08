use super::{
    assembly::Assembly, breakpoint_view::BreakpointView, memory_view::MemoryView,
    register_view::RegisterView, widget::KeyAction,
};
use crate::{
    cpu::CPU,
    debug::widget::{WidgetKind, WidgetList},
};
use crossterm::{
    event::{self, EnableMouseCapture, Event as CEvent},
    execute,
    terminal::{disable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

use event::DisableMouseCapture;
use std::{
    error::Error,
    io::{stdout, Stdout, Write},
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};
use tui::{backend::CrosstermBackend, Terminal};

enum Event<I> {
    Input(I),
    Tick,
}

/// The terminal-based graphic debugger for the GMB emulator.
pub struct Debugger {
    cpu: CPU,
    widgets: WidgetList,
    running: bool,
}

impl Debugger {
    /// Creates a new debugger from a `CPU`.
    pub fn new(cpu: CPU) -> Debugger {
        Debugger {
            cpu,
            widgets: WidgetList::new(),
            running: false,
        }
    }

    /// Refreshes the UI.
    pub fn refresh(&mut self) {
        self.widgets.refresh(&self.cpu);
    }

    fn draw(&mut self, terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> std::io::Result<()> {
        terminal.draw(|mut f| {
            self.widgets.draw(&mut f, &self.cpu);
        })
    }

    /// Runs the debugger, only stopping when an error occurs or when the user
    /// manually quits the application.
    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;

        let (tx, rx) = mpsc::channel();
        let tick_rate = Duration::from_millis(16);

        // Used to fix input on linux/unix platforms
        crossterm::terminal::enable_raw_mode().unwrap();

        thread::spawn(move || {
            let mut last_tick = Instant::now();
            loop {
                // poll for tick rate duration, if no events, sent tick event.
                if event::poll(tick_rate - last_tick.elapsed()).unwrap() {
                    if let CEvent::Key(key) = event::read().unwrap() {
                        tx.send(Event::Input(crossterm::event::Event::Key(key)))
                            .unwrap_or(());
                    }
                }
                if last_tick.elapsed() >= tick_rate {
                    last_tick = Instant::now();
                    tx.send(Event::Tick).unwrap_or(());
                }
            }
        });

        terminal.clear()?;

        self.widgets
            .add(Box::new(Assembly::new()), WidgetKind::Assembly);
        self.widgets
            .add(Box::new(MemoryView::new()), WidgetKind::Memory);
        self.widgets
            .add(Box::new(RegisterView::new()), WidgetKind::Registers);
        self.widgets
            .add(Box::new(BreakpointView::new()), WidgetKind::Breakpoints);
        self.widgets.select(Some(WidgetKind::Memory));

        loop {
            if self.running {
                self.running = match self.cpu.step_check() {
                    Ok(r) => {
                        if r {
                            self.refresh();
                            false
                        } else {
                            true
                        }
                    }
                    Err(e) => {
                        self.widgets.display_error(format!("{}", e));
                        self.refresh();
                        false
                    }
                }
            } else {
                self.draw(&mut terminal)?;
            }

            self.widgets
                .set_status(if self.running { "Running" } else { "Idle" });

            let input = match rx.try_recv() {
                Ok(e) => Some(e),
                Err(e) => match e {
                    mpsc::TryRecvError::Empty => None,
                    mpsc::TryRecvError::Disconnected => break,
                },
            };

            if let Some(result) = input {
                match result {
                    Event::Input(event) => match self.widgets.handle_key(event, &mut self.cpu) {
                        KeyAction::Pause => {
                            self.running = false;
                        }
                        KeyAction::Nothing => {
                            self.refresh();
                        }
                        KeyAction::Run => {
                            self.running = true;
                        }
                        KeyAction::Step => {
                            // TODO: handle error
                            if let Err(e) = self.cpu.step() {
                                self.widgets.display_error(format!("{}", e));
                                self.refresh();
                            }
                            self.refresh();
                        }
                        KeyAction::Quit => {
                            disable_raw_mode()?;
                            execute!(
                                terminal.backend_mut(),
                                LeaveAlternateScreen,
                                DisableMouseCapture
                            )?;
                            terminal.show_cursor()?;
                            break;
                        }
                    },
                    Event::Tick => {
                        if self.running {
                            self.refresh();
                            self.draw(&mut terminal)?;
                        }
                    }
                };
            }
        }

        Ok(())
    }
}
