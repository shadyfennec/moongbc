use crate::memory_map::Mem16;
use crate::{
    breakpoint::Breakpoint,
    memory_map::{Interconnect, Mem, MemIdx},
    Either,
};
use crate::{
    memory_map::SignedImm8,
    register::{Flag, Reg16, Reg8, Registers},
};
use crate::{
    opcode::{Decoder, Opcode},
    register::Condition,
};

/// CPU emulator, has access to the memory bus and the registers.
/// Its primary function is to execute instructions and modify memory
/// and registers accordingly.
#[derive(Default)]
pub struct CPU {
    pub(crate) registers: Registers,
    pub(crate) breakpoints: Vec<Breakpoint>,
    pub(crate) cycles: usize,
    pub(crate) interrupt_enable: bool,
}

// VBLANK timing: 17555

impl CPU {
    /// Creates a new CPU, initalizing the memory map and the registers.
    pub fn new() -> CPU {
        CPU {
            registers: Registers::new(),
            breakpoints: vec![],
            cycles: 0,
            interrupt_enable: false,
        }
    }

    /// Executes the current instruction, modifying registers and memory
    /// accordingly, and advances towards the next instruction.
    pub fn step(&mut self, memory: &mut Interconnect) {
        let instruction = Decoder::decode(&self.registers, memory);
        let size = instruction.size();

        let (maybe_pc, cycles) = self.execute_opcode(instruction, memory);

        memory.gpu.count_cycles(cycles);

        self.cycles += cycles;
        let new_pc =
            maybe_pc.unwrap_or_else(|| Reg16::PC.read(&self.registers).wrapping_add(size as u16));

        Reg16::PC.write(&mut self.registers, new_pc);
    }

    /// Executes one step of instruction, and returns true if a breakpoint is triggered
    pub fn step_check(&mut self, memory: &mut Interconnect) -> bool {
        self.step(memory);
        let results = self.check_breakpoints(memory);
        let mut breakpoints = self.breakpoints.clone();
        breakpoints.iter_mut().for_each(|b| {
            if let Breakpoint::Watch(w) = b {
                w.refresh(memory, &self.registers);
            }
        });
        self.breakpoints = breakpoints;
        results
    }

    /// Runs until an error occurs.
    pub fn run(&mut self, memory: &mut Interconnect) {
        loop {
            self.step(memory);
        }
    }

    pub fn check_breakpoints(&self, memory: &Interconnect) -> bool {
        self.breakpoints
            .iter()
            .map(|b| b.check(memory, &self.registers))
            .any(|b| b)
    }

    pub fn run_breakpoints(&mut self, memory: &mut Interconnect) {
        while !self.check_breakpoints(memory) {
            self.step(memory);
        }
    }

    pub fn add_breakpoint(&mut self, breakpoint: Breakpoint) {
        self.breakpoints.push(breakpoint);
    }

    // Special operations

    fn daa(&mut self) {
        let mut a = (Reg8::A.read(&self.registers)) as u16;

        let n = Flag::N.read(&self.registers);
        let c = Flag::C.read(&self.registers);
        let h = Flag::H.read(&self.registers);

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
                Flag::C.write(&mut self.registers, true);
            }
            if h || ((a & 0x0F) > 0x09) {
                a += 0x06;
            }
        }

        Flag::Z.write(&mut self.registers, (a as u8) == 0);
        Flag::H.write(&mut self.registers, false);
        Reg8::A.write(&mut self.registers, a as u8);
    }

    // Jumps, calls, returns

    // Returns the offset added to the next PC
    fn jump_relative(&self, condition: Option<Condition>, memory: &Interconnect) -> Option<i8> {
        let offset = SignedImm8.read(memory, &self.registers);

        match condition {
            Some(c) => {
                if c.read(&self.registers) {
                    Some(offset)
                } else {
                    None
                }
            }
            None => Some(offset),
        }
    }

    fn ret(&mut self, condition: Option<Condition>, memory: &Interconnect) -> Option<u16> {
        match condition {
            Some(c) => {
                if c.read(&self.registers) {
                    let low = Mem(MemIdx::Reg16(Reg16::SP)).read(memory, &self.registers) as u16;
                    Reg16::SP.add(&mut self.registers, 1);
                    let high = Mem(MemIdx::Reg16(Reg16::SP)).read(memory, &self.registers) as u16;
                    Reg16::SP.add(&mut self.registers, 1);

                    Some((high << 8) | low)
                } else {
                    None
                }
            }
            None => {
                let low = Mem(MemIdx::Reg16(Reg16::SP)).read(memory, &self.registers) as u16;
                Reg16::SP.add(&mut self.registers, 1);
                let high = Mem(MemIdx::Reg16(Reg16::SP)).read(memory, &self.registers) as u16;
                Reg16::SP.add(&mut self.registers, 1);

                Some((high << 8) | low)
            }
        }
    }

    fn jump(&mut self, condition: Option<Condition>, addr: u16) -> Option<u16> {
        match condition {
            Some(c) => {
                if c.read(&self.registers) {
                    Some(addr)
                } else {
                    None
                }
            }
            None => Some(addr),
        }
    }

    fn call(
        &mut self,
        condition: Option<Condition>,
        memory: &mut Interconnect,
        addr: u16,
    ) -> Option<u16> {
        match condition {
            Some(c) => {
                if c.read(&self.registers) {
                    self.push_direct(Reg16::PC.read(&self.registers) + 3, memory);
                    Some(addr)
                } else {
                    None
                }
            }
            None => {
                self.push_direct(Reg16::PC.read(&self.registers) + 3, memory);
                Some(addr)
            }
        }
    }

    // 16-bit operations

    fn increment_16(&mut self, reg: Reg16) {
        reg.add(&mut self.registers, 1)
    }

    fn decrement_16(&mut self, reg: Reg16) {
        reg.sub(&mut self.registers, 1)
    }

    fn add_16(&mut self, dst: Reg16, src: Reg16) {
        let a = dst.read(&self.registers) as u32;
        let b = src.read(&self.registers) as u32;

        let r = a + b;

        dst.write(&mut self.registers, r as u16);

        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, ((a ^ b ^ (r & 0xffff)) & 0x1000) != 0);
        Flag::C.write(&mut self.registers, (r & 0x10000) != 0);
    }

    fn add_16_sp(&mut self, dst: Reg16, src: SignedImm8, memory: &Interconnect) {
        let a = dst.read(&self.registers) as i32;
        let b = src.read(memory, &self.registers) as i32;

        let r = a + b;

        dst.write(&mut self.registers, r as u16);
        Flag::Z.write(&mut self.registers, false);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, ((a ^ b ^ (r & 0xffff)) & 0x1000) != 0);
        Flag::C.write(&mut self.registers, (r & 0x10000) != 0);
    }

    // 16-bit loads & stack operations

    fn load_16(&mut self, dst: Reg16, src: u16) {
        dst.write(&mut self.registers, src)
    }

    fn load_16_sp(&mut self, dst: Mem16, src: Reg16, memory: &mut Interconnect) {
        let value = src.read(&self.registers);

        dst.write(memory, &self.registers, value);
    }

    fn pop(&mut self, dst: Reg16, memory: &Interconnect) {
        let sp = Reg16::SP.read(&self.registers);
        let low = memory.read(sp) as u16;
        Reg16::SP.add(&mut self.registers, 1);

        let sp = Reg16::SP.read(&self.registers);
        let high = memory.read(sp) as u16;
        Reg16::SP.add(&mut self.registers, 1);

        dst.write(&mut self.registers, (high << 8) | low)
    }

    fn push(&mut self, src: Reg16, memory: &mut Interconnect) {
        let value = src.read(&self.registers);
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        Reg16::SP.sub(&mut self.registers, 1);
        memory.write(Reg16::SP.read(&self.registers), high);

        Reg16::SP.sub(&mut self.registers, 1);
        memory.write(Reg16::SP.read(&self.registers), low);
    }

    fn push_direct(&mut self, src: u16, memory: &mut Interconnect) {
        let value = src;
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        Reg16::SP.sub(&mut self.registers, 1);
        memory.write(Reg16::SP.read(&self.registers), high);

        Reg16::SP.sub(&mut self.registers, 1);
        memory.write(Reg16::SP.read(&self.registers), low);
    }

    fn load_hl_sp_offset(&mut self, memory: &Interconnect) {
        let base = Reg16::SP.read(&self.registers) as i32;
        let offset = SignedImm8.read(memory, &self.registers) as i32;

        let addr = base + offset;

        Reg16::HL.write(&mut self.registers, addr as u16);

        Flag::Z.write(&mut self.registers, false);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(
            &mut self.registers,
            ((base ^ offset ^ (addr & 0xFFFF)) & 0x10) == 0x10,
        );
        Flag::C.write(
            &mut self.registers,
            ((base ^ offset ^ (addr & 0xFFFF)) & 0x100) == 0x100,
        );
    }

    // Load

    fn load_mem(&mut self, dst: Mem, value: u8, memory: &mut Interconnect) {
        dst.write(memory, &self.registers, value);
    }

    fn load_reg(&mut self, dst: Reg8, value: u8) {
        dst.write(&mut self.registers, value);
    }

    // Arithmetic operations

    fn increment_reg8(&mut self, dst: Reg8) {
        let current = dst.read(&self.registers);
        let result = current.wrapping_add(1);

        dst.write(&mut self.registers, result);
        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, result.trailing_zeros() >= 4);
    }

    fn increment_mem(&mut self, dst: Mem, memory: &mut Interconnect) {
        let current = dst.read(memory, &self.registers);
        let result = current.wrapping_add(1);

        dst.write(memory, &mut self.registers, result);
        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, result.trailing_zeros() >= 4);
    }

    fn decrement_reg8(&mut self, dst: Reg8) {
        let current = dst.read(&self.registers);
        let result = current.wrapping_sub(1);

        dst.write(&mut self.registers, result);
        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, result.trailing_zeros() >= 4);
    }

    fn decrement_mem(&mut self, dst: Mem, memory: &mut Interconnect) {
        let current = dst.read(memory, &self.registers);
        let result = current.wrapping_sub(1);

        dst.write(memory, &mut self.registers, result);
        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, result.trailing_zeros() >= 4);
    }

    fn add(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        let (result, overflow) = a.overflowing_add(b);
        let c = a ^ b ^ result;

        dst.write(&mut self.registers, result);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, c.trailing_zeros() >= 4);
        Flag::C.write(&mut self.registers, overflow);
    }

    fn add_carry(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers) as u16;
        let b = src as u16;
        let c = Flag::C.read(&self.registers) as u16;

        let result = a + b + c;

        dst.write(&mut self.registers, result as u8);

        Flag::Z.write(&mut self.registers, (result as u8) == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, ((a & 0xF) + (b & 0xF) + c) > 0xF);
        Flag::C.write(&mut self.registers, result > 0xFF);
    }

    fn sub(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        let result = a.wrapping_sub(b);
        let c = a ^ b ^ result;

        dst.write(&mut self.registers, result);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, true);
        Flag::H.write(&mut self.registers, c.trailing_zeros() >= 4);
        Flag::C.write(&mut self.registers, c.trailing_zeros() >= 8);
    }

    fn sub_carry(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers) as i16;
        let b = src as i16;
        let c = Flag::C.read(&self.registers) as i16;

        let result = a.wrapping_sub(b).wrapping_sub(c);

        dst.write(&mut self.registers, result as u8);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, true);
        Flag::H.write(&mut self.registers, ((a & 0x0f) - (b & 0x0f) - c) < 0);
        Flag::C.write(&mut self.registers, result < 0);
    }

    fn and(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        let result = a & b;

        dst.write(&mut self.registers, result);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, true);
        Flag::C.write(&mut self.registers, false);
    }

    fn xor(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        let result = a ^ b;

        dst.write(&mut self.registers, result);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, false);
    }

    fn or(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        let result = a & b;

        dst.write(&mut self.registers, result);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, false);
    }

    fn compare(&mut self, dst: Reg8, src: u8) {
        let a = dst.read(&self.registers);
        let b = src;

        Flag::Z.write(&mut self.registers, a == b);
        Flag::N.write(&mut self.registers, true);
        Flag::H.write(&mut self.registers, (a.wrapping_sub(b) & 0xF) > (a & 0xF));
        Flag::C.write(&mut self.registers, a < b);
    }

    // Bit operations

    fn rlca(&mut self, memory: &mut Interconnect) {
        self.rlc(Either::Left(Reg8::A), memory);
        Flag::Z.write(&mut self.registers, false);
    }

    fn rrca(&mut self, memory: &mut Interconnect) {
        self.rrc(Either::Left(Reg8::A), memory);
        Flag::Z.write(&mut self.registers, false);
    }

    fn rla(&mut self, memory: &mut Interconnect) {
        self.rl(Either::Left(Reg8::A), memory);
        Flag::Z.write(&mut self.registers, false);
    }

    fn rra(&mut self, memory: &mut Interconnect) {
        self.rr(Either::Left(Reg8::A), memory);
        Flag::Z.write(&mut self.registers, false);
    }

    fn rlc(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };
        let result = value.rotate_left(1);

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 0x80) == 0x80);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn rrc(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };
        let result = value >> 1;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 1) == 1);
        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn rl(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };
        let old_c = Flag::C.read(&self.registers);

        let result = value.rotate_left(1);
        let result = if old_c { result | 1 } else { result & !(1) };

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 0x80) == 0x80);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn rr(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let old_c = (Flag::C.read(&self.registers) as u8) << 7;
        let result = (value >> 1) | old_c;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 1) == 1);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn sla(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let result = value << 1;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 0x80) == 0x80);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn sra(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let msb = value & 0x80;
        let result = (value >> 1) | msb;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 1) == 1);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn srl(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let result = value >> 1;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, (value & 1) == 1);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn swap(&mut self, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let high = value >> 4;
        let low = value & 0xF;

        let result = (low << 4) | high;

        Flag::Z.write(&mut self.registers, result == 0);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, false);
        Flag::C.write(&mut self.registers, false);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, result),
            Either::Right(m) => m.write(memory, &self.registers, result),
        };
    }

    fn bit(&mut self, bit: u8, dst: Either<Reg8, Mem>, memory: &Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let result = (value >> bit) & 1 == 0;

        Flag::Z.write(&mut self.registers, result);
        Flag::N.write(&mut self.registers, false);
        Flag::H.write(&mut self.registers, true);
    }

    fn set(&mut self, bit: u8, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let mask = 1 << bit;

        match dst {
            Either::Left(r) => r.write(&mut self.registers, value | mask),
            Either::Right(m) => m.write(memory, &self.registers, value | mask),
        };
    }

    fn res(&mut self, bit: u8, dst: Either<Reg8, Mem>, memory: &mut Interconnect) {
        let value = match dst {
            Either::Left(r) => r.read(&self.registers),
            Either::Right(m) => m.read(memory, &self.registers),
        };

        let mask = !(1 << bit);

        match dst {
            Either::Left(r) => r.write(&mut self.registers, value & mask),
            Either::Right(m) => m.write(memory, &self.registers, value & mask),
        };
    }

    /// Executes an instruction, and returns a potentially new
    /// address for the PC register. If the return value is `None`,
    /// then the new PC value is set to the next instruction, based
    /// on the instruction size in bytes.
    fn execute_opcode(
        &mut self,
        opcode: Opcode,
        memory: &mut Interconnect,
    ) -> (Option<u16>, usize) {
        let mut maybe_pc = None;
        let next_pc = Reg16::PC.read(&self.registers) + opcode.size() as u16;
        let (cycles, cycles_failure) = opcode.cycles();
        let mut actual_cycles = cycles;

        match opcode {
            Opcode::NOP => {}
            Opcode::STOP => unimplemented!(),
            Opcode::HALT => unimplemented!(),
            Opcode::DI => {
                self.interrupt_enable = false;
            }
            Opcode::EI => {
                self.interrupt_enable = true;
            }

            Opcode::JR(c) => {
                maybe_pc = self
                    .jump_relative(c, memory)
                    .map(|o| ((next_pc as i32) + (o as i32)) as u16);

                if let Some(c) = c {
                    if !c.read(&self.registers) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }

            Opcode::RET(c) => {
                maybe_pc = self.ret(c, memory);

                if let Some(c) = c {
                    if !c.read(&self.registers) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }
            Opcode::JP(c, a) => {
                maybe_pc = self.jump(c, a.read(memory, &self.registers));
                if let Some(c) = c {
                    if !c.read(&self.registers) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }
            Opcode::CALL(c, a) => {
                maybe_pc = self.call(c, memory, a.read(memory, &self.registers));
                if let Some(c) = c {
                    if !c.read(&self.registers) {
                        actual_cycles = cycles_failure.unwrap();
                    }
                }
            }
            Opcode::RETI => unimplemented!(),
            Opcode::JPHL => maybe_pc = self.jump(None, Reg16::HL.read(&self.registers)),
            Opcode::RST(_) => unimplemented!(),

            Opcode::INC16(r) => self.increment_16(r),
            Opcode::DEC16(r) => self.decrement_16(r),
            Opcode::ADDHL(dst, src) => self.add_16(dst, src),
            Opcode::ADDSP(r, o) => self.add_16_sp(r, o, memory),

            Opcode::LD16(dst, src) => self.load_16(dst, src.read(memory, &self.registers)),
            Opcode::LDImmMemSP(dst, src) => self.load_16_sp(dst, src, memory),
            Opcode::POP(r) => self.pop(r, memory),
            Opcode::PUSH(r) => self.push(r, memory),
            Opcode::LDHLSPOffset => self.load_hl_sp_offset(memory),
            Opcode::LDSPHL => self.load_16(Reg16::SP, Reg16::HL.read(&self.registers)),

            Opcode::LDARegMem(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),
            Opcode::LDHLMemIncA(dst, src) => {
                self.load_mem(dst, src.read(&self.registers), memory);
                Reg16::HL.add(&mut self.registers, 1)
            }
            Opcode::LDHLMemDecA(dst, src) => {
                self.load_mem(dst, src.read(&self.registers), memory);
                Reg16::HL.sub(&mut self.registers, 1)
            }
            Opcode::LDRegMemA(dst, src) => self.load_mem(dst, src.read(&self.registers), memory),
            Opcode::LDAHLMemInc(dst, src) => {
                self.load_reg(dst, src.read(memory, &self.registers));
                Reg16::HL.add(&mut self.registers, 1)
            }
            Opcode::LDAHLMemDec(dst, src) => {
                self.load_reg(dst, src.read(memory, &self.registers));
                Reg16::HL.sub(&mut self.registers, 1)
            }
            Opcode::LDRegImm(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),
            Opcode::LDHLMemImm(dst, src) => {
                self.load_mem(dst, src.read(memory, &self.registers), memory)
            }
            Opcode::LD(dst, src) => self.load_reg(dst, src.read(&self.registers)),
            Opcode::LDRegHLMem(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),
            Opcode::LDHLMemReg(dst, src) => self.load_mem(dst, src.read(&self.registers), memory),
            Opcode::LDHImmMemA(dst, src) => self.load_mem(dst, src.read(&self.registers), memory),
            Opcode::LDHAImmMem(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),
            Opcode::LDHCMemA(dst, src) => self.load_mem(dst, src.read(&self.registers), memory),
            Opcode::LDHACMem(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),
            Opcode::LDImmMemA(dst, src) => self.load_mem(dst, src.read(&self.registers), memory),
            Opcode::LDAImmMem(dst, src) => self.load_reg(dst, src.read(memory, &self.registers)),

            Opcode::INC(dst) => self.increment_reg8(dst),
            Opcode::INCHLMem(dst) => self.increment_mem(dst, memory),
            Opcode::DEC(dst) => self.decrement_reg8(dst),
            Opcode::DECHLMem(dst) => self.decrement_mem(dst, memory),
            Opcode::DAA => self.daa(),
            Opcode::SCF => {
                Flag::N.write(&mut self.registers, false);
                Flag::H.write(&mut self.registers, false);
                Flag::C.write(&mut self.registers, true);
            }
            Opcode::CPL => {
                let a = Reg8::A.read(&self.registers);
                Reg8::A.write(&mut self.registers, !a);
                Flag::N.write(&mut self.registers, true);
                Flag::H.write(&mut self.registers, true);
            }
            Opcode::CCF => {
                Flag::N.write(&mut self.registers, false);
                Flag::H.write(&mut self.registers, false);
                let c = Flag::C.read(&self.registers);
                Flag::C.write(&mut self.registers, !c);
            }
            Opcode::ADD(dst, src) => self.add(dst, src.read(&self.registers)),
            Opcode::ADDHLMem(dst, src) => self.add(dst, src.read(memory, &self.registers)),
            Opcode::ADDImm(dst, src) => self.add(dst, src.read(memory, &self.registers)),
            Opcode::ADC(dst, src) => self.add_carry(dst, src.read(&self.registers)),
            Opcode::ADCHLMem(dst, src) => self.add_carry(dst, src.read(memory, &self.registers)),
            Opcode::ADCImm(dst, src) => self.add_carry(dst, src.read(memory, &self.registers)),
            Opcode::SUB(dst, src) => self.sub(dst, src.read(&self.registers)),
            Opcode::SUBHLMem(dst, src) => self.sub(dst, src.read(memory, &self.registers)),
            Opcode::SUBImm(dst, src) => self.sub(dst, src.read(memory, &self.registers)),
            Opcode::SBC(dst, src) => self.sub_carry(dst, src.read(&self.registers)),
            Opcode::SBCHLMem(dst, src) => self.sub_carry(dst, src.read(memory, &self.registers)),
            Opcode::SBCImm(dst, src) => self.sub_carry(dst, src.read(memory, &self.registers)),
            Opcode::AND(dst, src) => self.and(dst, src.read(&self.registers)),
            Opcode::ANDHLMem(dst, src) => self.and(dst, src.read(memory, &self.registers)),
            Opcode::ANDImm(dst, src) => self.and(dst, src.read(memory, &self.registers)),
            Opcode::XOR(dst, src) => self.xor(dst, src.read(&self.registers)),
            Opcode::XORHLMem(dst, src) => self.xor(dst, src.read(memory, &self.registers)),
            Opcode::XORImm(dst, src) => self.xor(dst, src.read(memory, &self.registers)),
            Opcode::OR(dst, src) => self.or(dst, src.read(&self.registers)),
            Opcode::ORHLMem(dst, src) => self.or(dst, src.read(memory, &self.registers)),
            Opcode::ORImm(dst, src) => self.or(dst, src.read(memory, &self.registers)),
            Opcode::CP(dst, src) => self.compare(dst, src.read(&self.registers)),
            Opcode::CPHLMem(dst, src) => self.compare(dst, src.read(memory, &self.registers)),
            Opcode::CPImm(dst, src) => self.compare(dst, src.read(memory, &self.registers)),

            Opcode::RLCA => self.rlca(memory),
            Opcode::RLA => self.rla(memory),
            Opcode::RRCA => self.rrca(memory),
            Opcode::RRA => self.rra(memory),

            Opcode::Illegal => panic!("Illegal opcode"),

            Opcode::RLC(r) => self.rlc(Either::Left(r), memory),
            Opcode::RLCMem(r) => self.rlc(Either::Right(r), memory),
            Opcode::RRC(r) => self.rrc(Either::Left(r), memory),
            Opcode::RRCMem(r) => self.rrc(Either::Right(r), memory),
            Opcode::RL(r) => self.rl(Either::Left(r), memory),
            Opcode::RLMem(r) => self.rl(Either::Right(r), memory),
            Opcode::RR(r) => self.rr(Either::Left(r), memory),
            Opcode::RRMem(r) => self.rr(Either::Right(r), memory),
            Opcode::SLA(r) => self.sla(Either::Left(r), memory),
            Opcode::SLAMem(r) => self.sla(Either::Right(r), memory),
            Opcode::SRA(r) => self.sra(Either::Left(r), memory),
            Opcode::SRAMem(r) => self.sra(Either::Right(r), memory),
            Opcode::SWAP(r) => self.swap(Either::Left(r), memory),
            Opcode::SWAPMem(r) => self.swap(Either::Right(r), memory),
            Opcode::SRL(r) => self.srl(Either::Left(r), memory),
            Opcode::SRLMem(r) => self.srl(Either::Right(r), memory),
            Opcode::BIT(b, r) => self.bit(b, Either::Left(r), memory),
            Opcode::BITMem(b, r) => self.bit(b, Either::Right(r), memory),
            Opcode::RES(b, r) => self.res(b, Either::Left(r), memory),
            Opcode::RESMem(b, r) => self.res(b, Either::Right(r), memory),
            Opcode::SET(b, r) => self.set(b, Either::Left(r), memory),
            Opcode::SETMem(b, r) => self.set(b, Either::Right(r), memory),
        };
        (maybe_pc, actual_cycles)
    }

    /// Returns an iterator over the instructions, without
    /// executing them or modifying memory nor registers.
    pub fn preview_iterator<'a>(
        &'a self,
        start: u16,
        memory: &'a Interconnect,
    ) -> CPUPreviewIterator {
        CPUPreviewIterator {
            cpu: self,
            memory,
            pc: start,
        }
    }
}

/// An iterator helper for iterating over the instructions without modifying
/// them. Its primary use is to disassemble instructions anywhere in memory
/// without having to make the CPU jump to them.
pub struct CPUPreviewIterator<'a> {
    cpu: &'a CPU,
    memory: &'a Interconnect,
    pc: u16,
}

impl<'a> Iterator for CPUPreviewIterator<'a> {
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        let opcode: Option<Opcode> =
            Decoder::decode_with_context(&self.cpu.registers, self.memory, self.pc);

        if let Some(o) = opcode {
            let size = o.size();

            self.pc += size as u16;

            Some(o)
        } else {
            None
        }
    }
}
