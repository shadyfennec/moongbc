use crate::BitField;

use std::fmt;
use std::iter::repeat;

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

    pub fn read(&self, registers: &Registers) -> u8 {
        registers.get_8(self)
    }

    pub fn write(&self, registers: &mut Registers, value: u8) {
        registers.set_8(self, value);
    }

    pub fn add(&self, registers: &mut Registers, value: u8) {
        registers.set_8(self, registers.get_8(self).wrapping_add(value));
    }

    pub fn sub(&self, registers: &mut Registers, value: u8) {
        registers.set_8(self, registers.get_8(self).wrapping_sub(value));
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

    pub fn read(&self, registers: &Registers) -> u16 {
        registers.get_16(self)
    }

    pub fn write(&self, registers: &mut Registers, value: u16) {
        registers.set_16(self, value);
    }

    pub fn add(&self, registers: &mut Registers, value: u16) {
        registers.set_16(self, registers.get_16(self).wrapping_add(value));
    }

    pub fn sub(&self, registers: &mut Registers, value: u16) {
        registers.set_16(self, registers.get_16(self).wrapping_sub(value));
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
#[derive(Clone)]
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

    pub fn read(&self, registers: &Registers) -> bool {
        let flags: BitField = Reg8::F.read(registers).into();
        flags.get(self.bit())
    }

    pub fn write(&self, registers: &mut Registers, value: bool) {
        let mut flags: BitField = Reg8::F.read(registers).into();
        flags.set(self.bit(), value);
        Reg8::F.write(registers, flags.into())
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

/// Describes a condition, used in jumps, calls and returns.
/// It can be read to get the boolean value of the condition.
#[derive(Debug, Copy, Clone)]
pub enum Condition {
    NZ,
    NC,
    Z,
    C,
}

impl Condition {
    pub fn read(&self, registers: &Registers) -> bool {
        match self {
            Condition::NZ => !Flag::Z.read(registers),
            Condition::NC => !Flag::C.read(registers),
            Condition::Z => Flag::Z.read(registers),
            Condition::C => Flag::C.read(registers),
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Condition::NZ => write!(f, "NZ"),
            Condition::NC => write!(f, "NC"),
            Condition::Z => write!(f, "Z"),
            Condition::C => write!(f, "C"),
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
