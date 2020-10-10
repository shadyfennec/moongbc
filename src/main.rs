use moongbc::debug::Debugger;
use moongbc::{cpu::CPU, memory_map::Interconnect};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cpu = CPU::new();

    let mut memory = Interconnect::new();
    memory.set_cartridge("pkmn_yellow.gbc");

    let mut debugger = Debugger::new(cpu, memory);
    debugger.run()?;

    Ok(())
}
