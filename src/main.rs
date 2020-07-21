use moongbc::cpu::CPU;
use moongbc::debug::Debugger;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut cpu = CPU::new();
    cpu.set_cartridge("pkmn_yellow.gbc");

    let mut debugger = Debugger::new(cpu);
    debugger.run()?;

    Ok(())
}
