use crate::cartridge::Cartridge;
use crate::memory_map::{Mem, MemoryMap};
use crate::opcode::{Condition, Decoder, Opcode};
use crate::register::{Flag, Imm16, Reg16, Reg8, Registers};
use crate::{Dst, RWResult, ReadWriteError, Src};

use std::fmt;
use std::path::Path;

#[derive(Debug)]
pub struct RuntimeError(pub RuntimeErrorKind);

#[derive(Debug)]
pub enum RuntimeErrorKind {
    ReadWriteError(ReadWriteError),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            RuntimeErrorKind::ReadWriteError(e) => write!(f, "{}", e),
        }
    }
}

pub struct Watcher<S: Src<T> + fmt::Display, T> {
    src: S,
    original: T,
}

impl<S: Src<T> + fmt::Display, T: PartialEq> Watcher<S, T> {
    pub fn new(src: S, cpu: &CPU) -> Watcher<S, T> {
        let original = src.read(cpu);
        Watcher { src, original }
    }

    pub fn check(&self, cpu: &CPU) -> bool {
        self.src.read(cpu) != self.original
    }
}

impl<S: Src<T> + fmt::Display, T: PartialEq> fmt::Display for Watcher<S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.src)
    }
}

pub enum WatchKind {
    Reg8(Watcher<Reg8, u8>),
    Reg16(Watcher<Reg16, u16>),
    Mem(Watcher<Mem<u16>, u8>),
    Flag(Watcher<Flag, bool>),
}

impl WatchKind {
    pub fn check(&self, cpu: &CPU) -> bool {
        match self {
            WatchKind::Reg8(w) => w.check(cpu),
            WatchKind::Reg16(w) => w.check(cpu),
            WatchKind::Mem(w) => w.check(cpu),
            WatchKind::Flag(w) => w.check(cpu),
        }
    }
}

impl fmt::Display for WatchKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WatchKind::Reg8(w) => write!(f, "Watching register {}", w),
            WatchKind::Reg16(w) => write!(f, "Watching register {}", w),
            WatchKind::Mem(w) => write!(f, "Watching address {}", w),
            WatchKind::Flag(w) => write!(f, "Watching flag {}", w),
        }
    }
}

pub enum InterruptKind {
    VBlank,
    LCD,
    Timer,
    SerialIO,
    Joypad,
}

impl InterruptKind {
    pub fn check(&self, cpu: &CPU) -> bool {
        let mask = match self {
            InterruptKind::VBlank => 0x1,
            InterruptKind::LCD => 0x2,
            InterruptKind::Timer => 0x4,
            InterruptKind::SerialIO => 0x8,
            InterruptKind::Joypad => 0x10,
        };

        Mem(0xFF0F).read(cpu) & mask != 0
    }
}

impl fmt::Display for InterruptKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterruptKind::VBlank => write!(f, "Watching VBlank interrupt"),
            InterruptKind::LCD => write!(f, "Watching LCD interrupt"),
            InterruptKind::Timer => write!(f, "Watching Timer interrupt"),
            InterruptKind::SerialIO => write!(f, "Watching Serial I/O interrupt"),
            InterruptKind::Joypad => write!(f, "Watching Joypad interrupt"),
        }
    }
}

pub enum Breakpoint {
    Address(u16),
    Opcode(u8),
    Watch(WatchKind),
    Interrupt(InterruptKind),
}

impl Breakpoint {
    pub fn check(&self, cpu: &CPU) -> bool {
        match self {
            Breakpoint::Address(addr) => Reg16::PC.read(cpu) == *addr,
            Breakpoint::Opcode(op) => Mem(Reg16::PC).read(cpu) == *op,
            Breakpoint::Watch(w) => w.check(cpu),
            Breakpoint::Interrupt(i) => i.check(cpu),
        }
    }

    pub fn to_string(&self, cpu: &CPU) -> String {
        let reached = if self.check(cpu) { "(Reached)" } else { "" };

        format!("{} {}", self, reached)
    }
}

impl fmt::Display for Breakpoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Breakpoint::Address(addr) => write!(f, "Break at address 0x{:04x}", addr),
            Breakpoint::Opcode(op) => write!(f, "Break at opcode 0x{:02x}", op),
            Breakpoint::Watch(w) => write!(f, "{}", w.to_string()),
            Breakpoint::Interrupt(i) => write!(f, "{}", i),
        }
    }
}

/// CPU emulator, has access to the memory bus and the registers.
/// Its primary function is to execute instructions and modify memory
/// and registers accordingly.
#[derive(Default)]
pub struct CPU {
    pub(crate) registers: Registers,
    pub(crate) memory_map: MemoryMap,
    pub(crate) breakpoints: Vec<Breakpoint>,
    pub(crate) cycles: usize,
}

impl CPU {
    /// Creates a new CPU, initalizing the memory map and the registers.
    pub fn new() -> CPU {
        CPU {
            registers: Registers::new(),
            memory_map: MemoryMap::new(),
            breakpoints: vec![],
            cycles: 0,
        }
    }

    /// Initializes and "inserts" a game cartridge into the cartridge slot.
    /// The cartridge is ultimately managed by the `MBC`.
    pub fn set_cartridge<P: AsRef<Path>>(&mut self, path: P) {
        self.memory_map
            .set_cartridge(Cartridge::load(path).unwrap())
    }

    /// Executes the current instruction, modifying registers and memory
    /// accordingly, and advances towards the next instruction.
    pub fn step(&mut self) -> Result<(), RuntimeError> {
        let instruction = Decoder::decode(self);
        let size = instruction.size();

        let mut result = Ok(());
        let new_pc = match self.execute_opcode(instruction) {
            Ok((option, cycles)) => {
                self.cycles += cycles;
                option.unwrap_or_else(|| Reg16::PC.read(self).wrapping_add(size as u16))
            }
            Err(e) => {
                result = Err(e);
                Reg16::PC.read(self)
            }
        };

        Reg16::PC.write(self, new_pc);
        result
    }

    /// Executes one step of instruction, and returns true if a breakpoint is triggered
    pub fn step_check(&mut self) -> Result<bool, RuntimeError> {
        self.step()?;
        Ok(self.check_breakpoints())
    }

    /// Runs until an error occurs.
    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            self.step()?;
        }
    }

    pub fn check_breakpoints(&self) -> bool {
        self.breakpoints.iter().map(|b| b.check(self)).any(|b| b)
    }

    pub fn run_breakpoints(&mut self) -> Result<(), RuntimeError> {
        while !self.check_breakpoints() {
            self.step()?
        }
        Ok(())
    }

    pub fn add_breakpoint(&mut self, breakpoint: Breakpoint) {
        self.breakpoints.push(breakpoint);
    }

    // Special operations

    fn daa<D: Dst<u8> + Src<u8>>(&mut self, dst: D) -> RWResult<()> {
        let mut a = (dst.try_read(self)?) as u16;

        let n = Flag::N.try_read(self)?;
        let c = Flag::C.try_read(self)?;
        let h = Flag::H.try_read(self)?;

        if n {
            if c {
                a = a.wrapping_sub(0x60);
            }
            if h {
                a = a.wrapping_sub(0x06);
            }
        } else {
            if c || ((a & 0xFF) > 0x99) {
                a += 0x60;
                Flag::C.write(self, true);
            }
            if h || ((a & 0x0F) > 0x09) {
                a += 0x06;
            }
        }

        Flag::Z.try_write(self, (a as u8) == 0)?;
        Flag::H.try_write(self, false)?;
        dst.try_write(self, a as u8)?;

        Ok(())
    }

    // Jumps, calls, returns

    // Returns the offset added to the next PC
    fn jump_relative<S: Src<i8>>(
        &mut self,
        condition: Option<Condition>,
        offset: S,
    ) -> RWResult<Option<i8>> {
        let offset = offset.read(self);

        Ok(match condition {
            Some(c) => {
                if c.try_read(self)? {
                    Some(offset)
                } else {
                    None
                }
            }
            None => Some(offset),
        })
    }

    fn ret(&mut self, condition: Option<Condition>) -> RWResult<Option<u16>> {
        Ok(match condition {
            Some(c) => {
                if c.try_read(self)? {
                    let low = Mem(Reg16::SP).try_read(self)? as u16;
                    Reg16::SP.add(self, 1);
                    let high = Mem(Reg16::SP).try_read(self)? as u16;
                    Reg16::SP.add(self, 1);

                    Some((high << 8) | low)
                } else {
                    None
                }
            }
            None => {
                let low = Mem(Reg16::SP).try_read(self)? as u16;
                Reg16::SP.add(self, 1);
                let high = Mem(Reg16::SP).try_read(self)? as u16;
                Reg16::SP.add(self, 1);

                Some((high << 8) | low)
            }
        })
    }

    fn jump<S: Src<u16>>(
        &mut self,
        condition: Option<Condition>,
        addr: S,
    ) -> RWResult<Option<u16>> {
        Ok(match condition {
            Some(c) => {
                if c.read(self) {
                    Some(addr.try_read(self)?)
                } else {
                    None
                }
            }
            None => Some(addr.try_read(self)?),
        })
    }

    fn call<S: Src<u16>>(
        &mut self,
        condition: Option<Condition>,
        addr: S,
    ) -> RWResult<Option<u16>> {
        Ok(match condition {
            Some(c) => {
                if c.read(self) {
                    self.push(Reg16::PC.read(self) + 3)?;
                    Some(addr.try_read(self)?)
                } else {
                    None
                }
            }
            None => {
                self.push(Reg16::PC.read(self) + 3)?;
                Some(addr.try_read(self)?)
            }
        })
    }

    // 16-bit operations

    fn increment_16<D: Src<u16> + Dst<u16>>(&mut self, dst: D) -> RWResult<()> {
        dst.try_add(self, 1)
    }

    fn decrement_16<D: Src<u16> + Dst<u16>>(&mut self, dst: D) -> RWResult<()> {
        dst.try_sub(self, 1)
    }

    fn add_16<D: Dst<u16> + Src<u16>, S: Src<u16>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)? as u32;
        let b = src.try_read(self)? as u32;

        let r = a + b;

        dst.try_write(self, r as u16)?;

        Flag::N.write(self, false);
        Flag::H.write(self, ((a ^ b ^ (r & 0xffff)) & 0x1000) != 0);
        Flag::C.write(self, (r & 0x10000) != 0);

        Ok(())
    }

    fn add_16_sp<D: Dst<u16> + Src<u16>, S: Src<i8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)? as i32;
        let b = src.try_read(self)? as i32;

        let r = a + b;

        dst.try_write(self, r as u16)?;
        Flag::Z.write(self, false);
        Flag::N.write(self, false);
        Flag::H.write(self, ((a ^ b ^ (r & 0xffff)) & 0x1000) != 0);
        Flag::C.write(self, (r & 0x10000) != 0);

        Ok(())
    }

    // 16-bit loads & stack operations

    fn load_16<D: Dst<u16> + Src<u16>, S: Src<u16>>(&mut self, dst: D, src: S) -> RWResult<()> {
        dst.try_write(self, src.read(self))
    }

    fn load_16_sp<S: Src<u16>>(&mut self, dst: Mem<Imm16>, src: S) -> RWResult<()> {
        let value = src.try_read(self)?;
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        dst.try_write(self, low)?;
        Mem(dst.0.try_read(self)? + 1).try_write(self, high)
    }

    fn pop<D: Dst<u16>>(&mut self, dst: D) -> RWResult<()> {
        let low = Mem(Reg16::SP).try_read(self)? as u16;
        Reg16::SP.add(self, 1);
        let high = Mem(Reg16::SP).try_read(self)? as u16;
        Reg16::SP.add(self, 1);

        dst.try_write(self, (high << 8) | low)
    }

    fn push<S: Src<u16>>(&mut self, src: S) -> RWResult<()> {
        let value = src.read(self);
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        Reg16::SP.sub(self, 1);
        self.load(Mem(Reg16::SP), high)?;
        Reg16::SP.sub(self, 1);
        self.load(Mem(Reg16::SP), low)?;

        Ok(())
    }

    fn load_hl_sp_offset<S: Src<i8>>(&mut self, src: S) -> RWResult<()> {
        let base = Reg16::SP.try_read(self)? as i32;
        let offset = src.try_read(self)? as i32;

        let addr = base + offset;

        Reg16::HL.try_write(self, addr as u16)?;

        Flag::Z.write(self, false);
        Flag::N.write(self, false);
        Flag::H.write(self, ((base ^ offset ^ (addr & 0xFFFF)) & 0x10) == 0x10);
        Flag::C.write(self, ((base ^ offset ^ (addr & 0xFFFF)) & 0x100) == 0x100);

        Ok(())
    }

    // Load

    fn load<S: Src<u8>, D: Dst<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        dst.try_write(self, src.try_read(self)?)?;
        Ok(())
    }

    // Arithmetic operations

    fn increment<D: Dst<u8> + Src<u8>>(&mut self, dst: D) -> RWResult<()> {
        let current = dst.try_read(self)?;
        let result = current.wrapping_add(1);

        dst.try_write(self, result)?;
        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, result.trailing_zeros() >= 4);

        Ok(())
    }

    fn decrement<D: Dst<u8> + Src<u8>>(&mut self, dst: D) -> RWResult<()> {
        let current = dst.try_read(self)?;
        let result = current.wrapping_sub(1);

        dst.try_write(self, result)?;
        Flag::Z.write(self, result == 0);
        Flag::N.write(self, true);
        Flag::H.write(self, result.trailing_zeros() >= 4);

        Ok(())
    }

    fn add<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        let (result, overflow) = a.overflowing_add(b);
        let c = a ^ b ^ result;

        dst.try_write(self, result)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, c.trailing_zeros() >= 4);
        Flag::C.write(self, overflow);

        Ok(())
    }

    fn add_carry<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)? as u16;
        let b = src.try_read(self)? as u16;
        let c = Flag::C.read(self) as u16;

        let result = a + b + c;

        dst.try_write(self, result as u8)?;

        Flag::Z.write(self, (result as u8) == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, ((a & 0xF) + (b & 0xF) + c) > 0xF);
        Flag::C.write(self, result > 0xFF);

        Ok(())
    }

    fn sub<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        let result = a.wrapping_sub(b);
        let c = a ^ b ^ result;

        dst.try_write(self, result)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, true);
        Flag::H.write(self, c.trailing_zeros() >= 4);
        Flag::C.write(self, c.trailing_zeros() >= 8);

        Ok(())
    }

    fn sub_carry<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)? as i16;
        let b = src.try_read(self)? as i16;
        let c = Flag::C.read(self) as i16;

        let result = a.wrapping_sub(b).wrapping_sub(c);

        dst.try_write(self, result as u8)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, true);
        Flag::H.write(self, ((a & 0x0f) - (b & 0x0f) - c) < 0);
        Flag::C.write(self, result < 0);

        Ok(())
    }

    fn and<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        let result = a & b;

        dst.try_write(self, result)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, true);
        Flag::C.write(self, false);

        Ok(())
    }

    fn xor<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        let result = a ^ b;

        dst.try_write(self, result)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, false);

        Ok(())
    }

    fn or<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        let result = a & b;

        dst.try_write(self, result)?;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, false);

        Ok(())
    }

    fn compare<D: Dst<u8> + Src<u8>, S: Src<u8>>(&mut self, dst: D, src: S) -> RWResult<()> {
        let a = dst.try_read(self)?;
        let b = src.try_read(self)?;

        Flag::Z.write(self, a == b);
        Flag::N.write(self, true);
        Flag::H.write(self, (a.wrapping_sub(b) & 0xF) > (a & 0xF));
        Flag::C.write(self, a < b);

        Ok(())
    }

    // Bit operations

    fn rlca(&mut self) -> RWResult<()> {
        self.rlc(Reg8::A)?;
        Flag::Z.write(self, false);
        Ok(())
    }

    fn rrca(&mut self) -> RWResult<()> {
        self.rrc(Reg8::A)?;
        Flag::Z.write(self, false);
        Ok(())
    }

    fn rla(&mut self) -> RWResult<()> {
        self.rl(Reg8::A)?;
        Flag::Z.write(self, false);
        Ok(())
    }

    fn rra(&mut self) -> RWResult<()> {
        self.rr(Reg8::A)?;
        Flag::Z.write(self, false);
        Ok(())
    }

    fn rlc<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let result = value.rotate_left(1);

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 0x80) == 0x80);
        dst.try_write(self, result)
    }

    fn rrc<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let result = value >> 1;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 1) == 1);
        dst.try_write(self, result)
    }

    fn rl<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let old_c = Flag::C.read(self);

        let result = value.rotate_left(1);
        let result = if old_c { result | 1 } else { result & !(1) };

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 0x80) == 0x80);

        dst.try_write(self, value)
    }

    fn rr<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let old_c = (Flag::C.read(self) as u8) << 7;
        let result = (value >> 1) | old_c;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 1) == 1);
        dst.try_write(self, result)
    }

    fn sla<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let result = value << 1;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 0x80) == 0x80);

        dst.try_write(self, result)
    }

    fn sra<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let msb = value & 0x80;
        let result = (value >> 1) | msb;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 1) == 1);

        dst.try_write(self, result)
    }

    fn srl<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let result = value >> 1;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, (value & 1) == 1);

        dst.try_write(self, result)
    }

    fn swap<D: Src<u8> + Dst<u8>>(&mut self, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let high = value >> 4;
        let low = value & 0xF;

        let result = (low << 4) | high;

        Flag::Z.write(self, result == 0);
        Flag::N.write(self, false);
        Flag::H.write(self, false);
        Flag::C.write(self, false);

        dst.try_write(self, result)
    }

    fn bit<D: Src<u8> + Dst<u8>>(&mut self, bit: u8, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;

        let result = (value >> bit) & 1 == 0;

        Flag::Z.write(self, result);
        Flag::N.write(self, false);
        Flag::H.write(self, true);

        Ok(())
    }

    fn set<D: Src<u8> + Dst<u8>>(&mut self, bit: u8, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let mask = 1 << bit;

        dst.try_write(self, value | mask)
    }

    fn res<D: Src<u8> + Dst<u8>>(&mut self, bit: u8, dst: D) -> RWResult<()> {
        let value = dst.try_read(self)?;
        let mask = !(1 << bit);

        dst.try_write(self, value & mask)
    }

    /// Executes an instruction, and returns a potentially new
    /// address for the PC register. If the return value is `None`,
    /// then the new PC value is set to the next instruction, based
    /// on the instruction size in bytes.
    fn execute_opcode(&mut self, opcode: Opcode) -> Result<(Option<u16>, usize), RuntimeError> {
        let mut maybe_pc = None;
        let next_pc = Reg16::PC.read(self) + opcode.size() as u16;
        let (cycles, cycles_failure) = opcode.cycles();
        let mut actual_cycles = cycles;

        match opcode {
            Opcode::NOP => Ok(()),
            Opcode::STOP => unimplemented!(),
            Opcode::HALT => unimplemented!(),
            Opcode::DI => unimplemented!(),
            Opcode::EI => unimplemented!(),

            Opcode::JR(c, o) => self.jump_relative(c, o).map(|o| {
                maybe_pc = o.map(|v| ((next_pc as i32) + (v as i32)) as u16);
                if let Some(c) = c {
                    if !c.read(self) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }),
            Opcode::RET(c) => self.ret(c).map(|v| {
                maybe_pc = v;
                if let Some(c) = c {
                    if !c.read(self) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }),
            Opcode::JP(c, a) => self.jump(c, a).map(|v| {
                maybe_pc = v;
                if let Some(c) = c {
                    if !c.read(self) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }),
            Opcode::CALL(c, a) => self.call(c, a).map(|v| {
                maybe_pc = v;
                if let Some(c) = c {
                    if !c.read(self) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }),
            Opcode::RETI => unimplemented!(),
            Opcode::JPHL => self.jump(None, Reg16::HL).map(|v| maybe_pc = v),
            Opcode::RST(_) => unimplemented!(),

            Opcode::INC16(r) => self.increment_16(r),
            Opcode::DEC16(r) => self.decrement_16(r),
            Opcode::ADDHL(dst, src) => self.add_16(dst, src),
            Opcode::ADDSP(r, o) => self.add_16_sp(r, o),

            Opcode::LD16(dst, src) => self.load_16(dst, src),
            Opcode::LDImmMemSP(dst, src) => self.load_16_sp(dst, src),
            Opcode::POP(r) => self.pop(r),
            Opcode::PUSH(r) => self.push(r),
            Opcode::LDHLSPOffset(src) => self.load_hl_sp_offset(src),
            Opcode::LDSPHL => self.load_16(Reg16::SP, Reg16::HL),

            Opcode::LDARegMem(dst, src) => self.load(dst, src),
            Opcode::LDHLMemIncA(dst, src) => self.load(dst, src).and(Reg16::HL.try_add(self, 1)),
            Opcode::LDHLMemDecA(dst, src) => self.load(dst, src).and(Reg16::HL.try_sub(self, 1)),
            Opcode::LDRegMemA(dst, src) => self.load(dst, src),
            Opcode::LDAHLMemInc(dst, src) => self.load(dst, src).and(Reg16::HL.try_add(self, 1)),
            Opcode::LDAHLMemDec(dst, src) => self.load(dst, src).and(Reg16::HL.try_sub(self, 1)),
            Opcode::LDRegImm(dst, src) => self.load(dst, src),
            Opcode::LDHLMemImm(dst, src) => self.load(dst, src),
            Opcode::LD(dst, src) => self.load(dst, src),
            Opcode::LDRegHLMem(dst, src) => self.load(dst, src),
            Opcode::LDHLMemReg(dst, src) => self.load(dst, src),
            Opcode::LDHImmMemA(dst, src) => self.load(dst, src),
            Opcode::LDHAImmMem(dst, src) => self.load(dst, src),
            Opcode::LDHCMemA(dst, src) => self.load(dst, src),
            Opcode::LDHACMem(dst, src) => self.load(dst, src),
            Opcode::LDImmMemA(dst, src) => self.load(dst, src),
            Opcode::LDAImmMem(dst, src) => self.load(dst, src),

            Opcode::INC(dst) => self.increment(dst),
            Opcode::INCHLMem(dst) => self.increment(dst),
            Opcode::DEC(dst) => self.decrement(dst),
            Opcode::DECHLMem(dst) => self.decrement(dst),
            Opcode::DAA => self.daa(Reg8::A),
            Opcode::SCF => {
                Flag::N.write(self, false);
                Flag::H.write(self, false);
                Flag::C.write(self, true);
                Ok(())
            }
            Opcode::CPL => {
                let a = Reg8::A.read(self);
                Reg8::A.write(self, !a);
                Flag::N.write(self, true);
                Flag::H.write(self, true);
                Ok(())
            }
            Opcode::CCF => {
                Flag::N.write(self, false);
                Flag::H.write(self, false);
                Flag::C.write(self, !Flag::C.read(self));
                Ok(())
            }
            Opcode::ADD(dst, src) => self.add(dst, src),
            Opcode::ADDHLMem(dst, src) => self.add(dst, src),
            Opcode::ADDImm(dst, src) => self.add(dst, src),
            Opcode::ADC(dst, src) => self.add_carry(dst, src),
            Opcode::ADCHLMem(dst, src) => self.add_carry(dst, src),
            Opcode::ADCImm(dst, src) => self.add_carry(dst, src),
            Opcode::SUB(dst, src) => self.sub(dst, src),
            Opcode::SUBHLMem(dst, src) => self.sub(dst, src),
            Opcode::SUBImm(dst, src) => self.sub(dst, src),
            Opcode::SBC(dst, src) => self.sub_carry(dst, src),
            Opcode::SBCHLMem(dst, src) => self.sub_carry(dst, src),
            Opcode::SBCImm(dst, src) => self.sub_carry(dst, src),
            Opcode::AND(dst, src) => self.and(dst, src),
            Opcode::ANDHLMem(dst, src) => self.and(dst, src),
            Opcode::ANDImm(dst, src) => self.and(dst, src),
            Opcode::XOR(dst, src) => self.xor(dst, src),
            Opcode::XORHLMem(dst, src) => self.xor(dst, src),
            Opcode::XORImm(dst, src) => self.xor(dst, src),
            Opcode::OR(dst, src) => self.or(dst, src),
            Opcode::ORHLMem(dst, src) => self.or(dst, src),
            Opcode::ORImm(dst, src) => self.or(dst, src),
            Opcode::CP(dst, src) => self.compare(dst, src),
            Opcode::CPHLMem(dst, src) => self.compare(dst, src),
            Opcode::CPImm(dst, src) => self.compare(dst, src),

            Opcode::RLCA => self.rlca(),
            Opcode::RLA => self.rla(),
            Opcode::RRCA => self.rrca(),
            Opcode::RRA => self.rra(),

            Opcode::Illegal => panic!("Illegal opcode"),

            Opcode::RLC(r) => self.rlc(r),
            Opcode::RLCMem(r) => self.rlc(r),
            Opcode::RRC(r) => self.rrc(r),
            Opcode::RRCMem(r) => self.rrc(r),
            Opcode::RL(r) => self.rl(r),
            Opcode::RLMem(r) => self.rl(r),
            Opcode::RR(r) => self.rr(r),
            Opcode::RRMem(r) => self.rr(r),
            Opcode::SLA(r) => self.sla(r),
            Opcode::SLAMem(r) => self.sla(r),
            Opcode::SRA(r) => self.sra(r),
            Opcode::SRAMem(r) => self.sra(r),
            Opcode::SWAP(r) => self.swap(r),
            Opcode::SWAPMem(r) => self.swap(r),
            Opcode::SRL(r) => self.srl(r),
            Opcode::SRLMem(r) => self.srl(r),
            Opcode::BIT(b, r) => self.bit(b, r),
            Opcode::BITMem(b, r) => self.bit(b, r),
            Opcode::RES(b, r) => self.res(b, r),
            Opcode::RESMem(b, r) => self.res(b, r),
            Opcode::SET(b, r) => self.set(b, r),
            Opcode::SETMem(b, r) => self.set(b, r),
        }
        .map(|_| (maybe_pc, actual_cycles))
        .map_err(|e| RuntimeError(RuntimeErrorKind::ReadWriteError(e)))
    }

    /// Returns an iterator over the instructions, without
    /// executing them or modifying memory nor registers.
    pub fn preview_iterator(&self, start: u16) -> CPUPreviewIterator {
        CPUPreviewIterator {
            cpu: self,
            pc: start,
        }
    }
}

/// An iterator helper for iterating over the instructions without modifying
/// them. Its primary use is to disassemble instructions anywhere in memory
/// without having to make the CPU jump to them.
pub struct CPUPreviewIterator<'a> {
    cpu: &'a CPU,
    pc: u16,
}

impl<'a> Iterator for CPUPreviewIterator<'a> {
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        let opcode: Option<Opcode> = Decoder::decode_with_context(&self.cpu, self.pc);

        if let Some(o) = opcode {
            let size = o.size();

            self.pc += size as u16;

            Some(o)
        } else {
            None
        }
    }
}

impl Iterator for CPU {
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        let opcode: Opcode = Decoder::decode(self);
        let size = opcode.size();

        Reg16::PC.add(self, size as u16);

        Some(opcode)
    }
}
