use moongbc::{cpu::CPU, memory_map::Interconnect};
use moongbc::{debug::Debugger, gui::GUI};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cpu = CPU::new();

    let mut memory = Interconnect::new();
    memory.set_cartridge("pkmn_yellow.gbc");

    let gui = GUI::new();

    let mut debugger = Debugger::new(cpu, memory, gui);
    debugger.run()?;

    Ok(())
}
