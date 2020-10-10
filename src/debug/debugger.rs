use super::{
    assembly::Assembly, breakpoint_view::BreakpointView, memory_view::MemoryView,
    register_view::RegisterView, widget::KeyAction,
};
use crate::{
    cpu::CPU,
    debug::widget::{WidgetKind, WidgetList},
    gui::GUI,
    memory_map::Interconnect,
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
    memory: Interconnect,
    gui: GUI,
    widgets: WidgetList,
    running: bool,
}

impl Debugger {
    /// Creates a new debugger from a `CPU`.
    pub fn new(cpu: CPU, memory: Interconnect, gui: GUI) -> Debugger {
        Debugger {
            cpu,
            memory,
            gui,
            widgets: WidgetList::new(),
            running: false,
        }
    }

    /// Refreshes the UI.
    pub fn refresh(&mut self) {
        self.widgets
            .set_status(if self.running { "Running" } else { "Idle" });
        self.widgets.refresh(&self.cpu, &self.memory);
    }

    fn draw(&mut self, terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> std::io::Result<()> {
        terminal.draw(|mut f| {
            self.widgets.draw(&mut f, &self.cpu, &self.memory);
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
            // In order to not hog the CPU when idle, I change the way
            // I poll for events depending on the state of the emulator:
            //
            // - If the emulator is running, I receive events without
            //   blocking, enabling us to have the speed required for the
            //   emulated CPU
            // - If the emulator is idle, I receive events with blocking;
            //   since we have an event for each 16ms, it draws the UI at
            //   60fps without constantly checking for events, which used
            //   the entire CPU for nothing.
            let event = if self.running {
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

            if self.running {
                self.running = !self.cpu.step_check(&mut self.memory);
                self.memory.gpu_step(&mut self.gui);
            }

            if let Some(e) = event {
                match e {
                    Event::Input(event) => {
                        match self
                            .widgets
                            .handle_key(event, &mut self.cpu, &mut self.memory)
                        {
                            KeyAction::Pause => {
                                self.running = false;
                            }
                            KeyAction::Nothing => {}
                            KeyAction::Run => {
                                self.running = true;
                            }
                            KeyAction::Step => {
                                // TODO: handle error
                                self.cpu.step(&mut self.memory);
                                self.memory.gpu_step(&mut self.gui);
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
                        }
                    }
                    Event::Tick => {
                        self.refresh();
                        self.draw(&mut terminal)?;
                    }
                };
            }
        }

        Ok(())
    }
}
