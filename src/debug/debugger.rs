use super::{
    assembly::Assembly, breakpoint_view::BreakpointView, memory_view::MemoryView,
    register_view::RegisterView,
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

/// The action to take after certain keys are pressed on a global scale, if any
pub enum KeyAction {
    // Quit the application
    Quit,
    // Run the CPU until further keys are pressed
    Run,
    // Run the CPU indefinitely
    RunToEnd,
    // Step once the CPU
    Step,
    // Wait for a key to be pressed
    Wait,
    // Explicitly stop running
    Stop,
}

/// The terminal-based graphic debugger for the GMB emulator.
pub struct Debugger {
    cpu: CPU,
    widgets: WidgetList,
}

impl Debugger {
    /// Creates a new debugger from a `CPU`.
    pub fn new(cpu: CPU) -> Debugger {
        Debugger {
            cpu,
            widgets: WidgetList::new(),
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
        let tick_rate = Duration::from_millis(250);

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

        let mut running = false;

        loop {
            if running {
                self.cpu.step();
                if self.cpu.check_breakpoints() {
                    running = false;
                }
                self.widgets.refresh(&self.cpu);
            }

            self.draw(&mut terminal)?;

            let result = if running {
                match rx.try_recv() {
                    Ok(e) => Some(e),
                    Err(e) => match e {
                        mpsc::TryRecvError::Empty => None,
                        mpsc::TryRecvError::Disconnected => break,
                    },
                }
            } else {
                match rx.recv() {
                    Ok(e) => Some(e),
                    Err(_) => break,
                }
            };

            if let Some(result) = result {
                match result {
                    Event::Input(event) => match self.widgets.handle_key(event, &mut self.cpu) {
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
                        KeyAction::Run => {
                            running = true;
                        }
                        KeyAction::Step => {
                            self.cpu.step();
                            self.widgets.refresh(&self.cpu);
                        }
                        KeyAction::Wait => {}
                        KeyAction::Stop => {
                            running = false;
                        }
                        KeyAction::RunToEnd => {
                            running = false;
                            self.cpu.run_breakpoints();
                            self.refresh();
                        }
                    },
                    Event::Tick => {
                        //self.widgets();
                    }
                }
            }
        }

        Ok(())
    }
}
