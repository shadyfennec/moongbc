use crate::cpu::CPU;
use crate::memory_map::Mem;
use crate::{BitField, Dst, Src};

use std::fmt;
use std::iter::repeat;

/// An immediate value, usually represented by a byte after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct Imm8;

impl Src<u8> for Imm8 {
    fn try_read(&self, cpu: &CPU) -> Option<u8> {
        Mem(Reg16::PC.read(cpu) + 1).try_read(cpu)
    }
}

// An immediate 16-bit value, usually represented by two bytes after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct Imm16;

impl Src<u16> for Imm16 {
    fn try_read(&self, cpu: &CPU) -> Option<u16> {
        let low = Mem(Reg16::PC.read(cpu) + 1).try_read(cpu);
        let high = Mem(Reg16::PC.read(cpu) + 2).try_read(cpu);

        match (low, high) {
            (Some(low), Some(high)) => Some(((high as u16) << 8) | low as u16),
            _ => None,
        }
    }
}

/// A signed immediate value, usually represented by two bytes after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct SignedImm8;

impl Src<i8> for SignedImm8 {
    fn try_read(&self, cpu: &CPU) -> Option<i8> {
        Imm8.try_read(cpu).map(|i| i as i8)
    }
}

/// An 8-bit register, representing either a high or low part of the 16-bit registers.
#[derive(Debug, Copy, Clone)]
pub enum Reg8 {
    B,
    C,
    D,
    E,
    H,
    L,
    A,
    F,
}

impl Reg8 {
    /// Returns the index of the register in the register vector.
    pub fn index(&self) -> usize {
        match self {
            Reg8::B | Reg8::C => 0,
            Reg8::D | Reg8::E => 1,
            Reg8::H | Reg8::L => 2,
            Reg8::A | Reg8::F => 3,
        }
    }

    /// Returns the mask used for retrieving the value of the regsiter:
    /// `$0xFF00` for the high part, `$0x00FF` for the low part.
    pub fn mask(&self) -> u16 {
        match self {
            Reg8::B | Reg8::D | Reg8::H | Reg8::A => 0xFF00,
            Reg8::C | Reg8::E | Reg8::L | Reg8::F => 0x00FF,
        }
    }

    /// Returns the number of bits to shift right to get
    /// the value of the register: 8 for the high part, 0 for
    /// the low part.
    pub fn shift(&self) -> u8 {
        match self {
            Reg8::B | Reg8::D | Reg8::H | Reg8::A => 8,
            Reg8::C | Reg8::E | Reg8::L | Reg8::F => 0,
        }
    }
}

impl Src<u8> for Reg8 {
    fn try_read(&self, cpu: &CPU) -> Option<u8> {
        Some(cpu.registers.get_8(self))
    }
}

impl Dst<u8> for Reg8 {
    fn try_write(&self, cpu: &mut CPU, value: u8) -> Result<(), String> {
        cpu.registers.set_8(self, value);
        Ok(())
    }
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg8::B => write!(f, "B"),
            Reg8::C => write!(f, "C"),
            Reg8::D => write!(f, "D"),
            Reg8::E => write!(f, "E"),
            Reg8::H => write!(f, "H"),
            Reg8::L => write!(f, "L"),
            Reg8::A => write!(f, "A"),
            Reg8::F => write!(f, "F"),
        }
    }
}

/// A 16-bit register of the CPU.
#[derive(Debug, Copy, Clone)]
pub enum Reg16 {
    BC,
    DE,
    HL,
    AF,
    SP,
    PC,
}

impl Reg16 {
    /// Returns the index of the register in the register vector.
    pub fn index(&self) -> usize {
        match self {
            Reg16::BC => 0,
            Reg16::DE => 1,
            Reg16::HL => 2,
            Reg16::AF => 3,
            Reg16::SP => 4,
            Reg16::PC => 5,
        }
    }
}

impl Src<u16> for Reg16 {
    fn try_read(&self, cpu: &CPU) -> Option<u16> {
        Some(cpu.registers.get_16(self))
    }
}

impl Dst<u16> for Reg16 {
    fn try_write(&self, cpu: &mut CPU, value: u16) -> Result<(), String> {
        cpu.registers.set_16(self, value);
        Ok(())
    }
}

impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg16::BC => write!(f, "BC"),
            Reg16::DE => write!(f, "DE"),
            Reg16::HL => write!(f, "HL"),
            Reg16::AF => write!(f, "AF"),
            Reg16::SP => write!(f, "SP"),
            Reg16::PC => write!(f, "PC"),
        }
    }
}

/// The different flags of the F register.
pub enum Flag {
    Z,
    N,
    H,
    C,
}

impl Flag {
    /// Returns the bit that represents the flag.
    pub fn bit(&self) -> u8 {
        match self {
            Flag::Z => 7,
            Flag::N => 6,
            Flag::H => 5,
            Flag::C => 4,
        }
    }
}

impl Src<bool> for Flag {
    fn try_read(&self, cpu: &CPU) -> Option<bool> {
        let flags: BitField = Reg8::F.read(cpu).into();
        Some(flags.get(self.bit()))
    }
}

impl Dst<bool> for Flag {
    fn try_write(&self, cpu: &mut CPU, value: bool) -> Result<(), String> {
        let mut flags: BitField = Reg8::F.read(cpu).into();
        flags.set(self.bit(), value);
        Reg8::F.try_write(cpu, flags.into())
    }
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Flag::Z => write!(f, "Z"),
            Flag::N => write!(f, "N"),
            Flag::H => write!(f, "H"),
            Flag::C => write!(f, "C"),
        }
    }
}

/// Holds the different registers of the CPU.
#[derive(Default)]
pub struct Registers {
    registers: Vec<u16>,
}

impl Registers {
    /// Creates the registers
    pub fn new() -> Registers {
        Registers {
            registers: repeat(0).take(6).collect(),
        }
    }

    /// Returns the value of a 8-bit register.
    pub(crate) fn get_8(&self, reg: &Reg8) -> u8 {
        let index = reg.index();
        let mask = reg.mask();
        let shift = reg.shift();

        ((self.registers[index] & mask) >> shift) as u8
    }

    /// Writes a value to a 8-bit register.
    pub(crate) fn set_8(&mut self, reg: &Reg8, value: u8) {
        let index = reg.index();
        let mask = reg.mask();
        let shift = reg.shift();

        let result = (self.registers[index] & (!mask)) | ((value as u16) << shift);
        self.registers[index] = result;
    }

    /// Returns the value of a 16-bit register.
    pub(crate) fn get_16(&self, reg: &Reg16) -> u16 {
        self.registers[reg.index()]
    }

    /// Writes a value to a 16-bit register.
    pub(crate) fn set_16(&mut self, reg: &Reg16, value: u16) {
        self.registers[reg.index()] = value;
    }

    pub(crate) fn get_flag(&self, flag: &Flag) -> bool {
        let bf: BitField = self.get_8(&Reg8::F).into();
        bf.get(flag.bit())
    }

    pub(crate) fn set_flag(&mut self, flag: &Flag, value: bool) {
        let mut bf: BitField = self.get_8(&Reg8::F).into();
        bf.set(flag.bit(), value);
        self.set_8(&Reg8::F, bf.into());
    }
}
