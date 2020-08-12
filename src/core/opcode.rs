use crate::cpu::CPU;
use crate::memory_map::{IOMem, Mem, Mem16};

use crate::register::{Flag, Imm16, Imm8, Reg16, Reg8, SignedImm8};
use crate::{Either, ReadWriteError, Src};
use std::fmt;

/// Describes a condition, used in jumps, calls and returns.
/// It can be read to get the boolean value of the condition.
#[derive(Debug, Copy, Clone)]
pub enum Condition {
    NZ,
    NC,
    Z,
    C,
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

impl Src<bool> for Condition {
    fn try_read(&self, cpu: &CPU) -> Result<bool, ReadWriteError> {
        match self {
            Condition::NZ => Flag::Z.try_read(cpu).map(|b| !b),
            Condition::NC => Flag::C.try_read(cpu).map(|b| !b),
            Condition::Z => Flag::Z.try_read(cpu),
            Condition::C => Flag::C.try_read(cpu),
        }
    }
}

/// A Decoder, used to translate bytes into the corresponding
/// instruction.
pub struct Decoder;

impl Decoder {
    /// Parses a CB instruction byte into its opcode.
    fn parse_cb(cpu: &CPU, pc: u16) -> Opcode {
        let value = Mem(pc).read(cpu);
        let location: Either<Reg8, Mem<Reg16>> = match value % 8 {
            0 => Either::Left(Reg8::B),
            1 => Either::Left(Reg8::C),
            2 => Either::Left(Reg8::D),
            3 => Either::Left(Reg8::E),
            4 => Either::Left(Reg8::H),
            5 => Either::Left(Reg8::L),
            6 => Either::Right(Mem(Reg16::HL)),
            7 => Either::Left(Reg8::A),
            _ => unreachable!(),
        };

        let bit = (value / 8) % 8;

        match value {
            0x00..=0x05 | 0x07 => Opcode::RLC(location.left().unwrap()),
            0x06 => Opcode::RLCMem(location.right().unwrap()),
            0x08..=0x0D | 0x0F => Opcode::RRC(location.left().unwrap()),
            0x0E => Opcode::RRCMem(location.right().unwrap()),
            0x10..=0x15 | 0x17 => Opcode::RL(location.left().unwrap()),
            0x16 => Opcode::RLMem(location.right().unwrap()),
            0x18..=0x1D | 0x1F => Opcode::RR(location.left().unwrap()),
            0x1E => Opcode::RRMem(location.right().unwrap()),
            0x20..=0x25 | 0x27 => Opcode::SLA(location.left().unwrap()),
            0x26 => Opcode::SLAMem(location.right().unwrap()),
            0x28..=0x2D | 0x2F => Opcode::SRA(location.left().unwrap()),
            0x2E => Opcode::SRAMem(location.right().unwrap()),
            0x30..=0x35 | 0x37 => Opcode::SWAP(location.left().unwrap()),
            0x36 => Opcode::SWAPMem(location.right().unwrap()),
            0x38..=0x3D | 0x3F => Opcode::SRL(location.left().unwrap()),
            0x3E => Opcode::SRLMem(location.right().unwrap()),
            0x40..=0x45
            | 0x47..=0x4D
            | 0x4F
            | 0x50..=0x55
            | 0x57..=0x5D
            | 0x5F
            | 0x60..=0x65
            | 0x67..=0x6D
            | 0x6F
            | 0x70..=0x75
            | 0x77..=0x7D
            | 0x7F => Opcode::BIT(bit, location.left().unwrap()),
            0x46 | 0x4E | 0x56 | 0x5E | 0x66 | 0x6E | 0x76 | 0x7E => {
                Opcode::BITMem(bit, location.right().unwrap())
            }
            0x80..=0x85
            | 0x87..=0x8D
            | 0x8F
            | 0x90..=0x95
            | 0x97..=0x9D
            | 0x9F
            | 0xA0..=0xA5
            | 0xA7..=0xAD
            | 0xAF
            | 0xB0..=0xB5
            | 0xB7..=0xBD
            | 0xBF => Opcode::RES(bit, location.left().unwrap()),
            0x86 | 0x8E | 0x96 | 0x9E | 0xA6 | 0xAE | 0xB6 | 0xBE => {
                Opcode::RESMem(bit, location.right().unwrap())
            }
            0xC0..=0xC5
            | 0xC7..=0xCD
            | 0xCF
            | 0xD0..=0xD5
            | 0xD7..=0xDD
            | 0xDF
            | 0xE0..=0xE5
            | 0xE7..=0xED
            | 0xEF
            | 0xF0..=0xF5
            | 0xF7..=0xFD
            | 0xFF => Opcode::SET(bit, location.left().unwrap()),
            0xC6 | 0xCE | 0xD6 | 0xDE | 0xE6 | 0xEE | 0xF6 | 0xFE => {
                Opcode::SETMem(bit, location.right().unwrap())
            }
        }
    }

    /// Decodes the current instruction pointed to by the PC register, and returns
    /// it.
    pub fn decode(cpu: &CPU) -> Opcode {
        Decoder::decode_with_context(cpu, Reg16::PC.read(cpu)).unwrap()
    }

    /// Decodes the instruction pointed to by a provided address in memory.
    pub fn decode_with_context(cpu: &CPU, pc: u16) -> Option<Opcode> {
        let value = Mem(pc).try_read(cpu);

        match value {
            Err(_) => None,
            Ok(value) => {
                let reg16_arithmetic: Reg16 = match (value / 0x10) % 4 {
                    0 => Reg16::BC,
                    1 => Reg16::DE,
                    2 => Reg16::HL,
                    3 => Reg16::SP,
                    _ => unreachable!(),
                };

                let reg16_stack = match (value / 0x10) % 4 {
                    0 => Reg16::BC,
                    1 => Reg16::DE,
                    2 => Reg16::HL,
                    3 => Reg16::AF,
                    _ => unreachable!(),
                };

                let dst: Either<Reg8, Mem<Reg16>> = match (value / 8) % 8 {
                    0 => Either::Left(Reg8::B),
                    1 => Either::Left(Reg8::C),
                    2 => Either::Left(Reg8::D),
                    3 => Either::Left(Reg8::E),
                    4 => Either::Left(Reg8::H),
                    5 => Either::Left(Reg8::L),
                    6 => Either::Right(Mem(Reg16::HL)),
                    7 => Either::Left(Reg8::A),
                    _ => unreachable!(),
                };

                let src: Either<Reg8, Mem<Reg16>> = match value % 8 {
                    0 => Either::Left(Reg8::B),
                    1 => Either::Left(Reg8::C),
                    2 => Either::Left(Reg8::D),
                    3 => Either::Left(Reg8::E),
                    4 => Either::Left(Reg8::H),
                    5 => Either::Left(Reg8::L),
                    6 => Either::Right(Mem(Reg16::HL)),
                    7 => Either::Left(Reg8::A),
                    _ => unreachable!(),
                };

                let rst_vector = ((value % 0x40) / 8) * 8;

                Some(match value {
                    0x00 => Opcode::NOP,
                    0x10 => Opcode::STOP,
                    0x20 => Opcode::JR(Some(Condition::NZ), SignedImm8),
                    0x30 => Opcode::JR(Some(Condition::NC), SignedImm8),
                    0x01 | 0x11 | 0x21 | 0x31 => Opcode::LD16(reg16_arithmetic, Imm16),
                    0x02 | 0x12 => Opcode::LDRegMemA(Mem(reg16_arithmetic), Reg8::A),
                    0x22 => Opcode::LDHLMemIncA(Mem(Reg16::HL), Reg8::A),
                    0x32 => Opcode::LDHLMemDecA(Mem(Reg16::HL), Reg8::A),
                    0x03 | 0x13 | 0x23 | 0x33 => Opcode::INC16(reg16_arithmetic),
                    0x04 | 0x14 | 0x24 => Opcode::INC(dst.left().unwrap()),
                    0x34 => Opcode::INCHLMem(Mem(Reg16::HL)),
                    0x05 | 0x15 | 0x25 => Opcode::DEC(dst.left().unwrap()),
                    0x35 => Opcode::DECHLMem(Mem(Reg16::HL)),
                    0x06 | 0x16 | 0x26 => Opcode::LDRegImm(dst.left().unwrap(), Imm8),
                    0x36 => Opcode::LDHLMemImm(Mem(Reg16::HL), Imm8),
                    0x07 => Opcode::RLCA,
                    0x17 => Opcode::RLA,
                    0x27 => Opcode::DAA,
                    0x37 => Opcode::SCF,
                    0x08 => Opcode::LDImmMemSP(Mem(Imm16), Reg16::SP),
                    0x18 => Opcode::JR(None, SignedImm8),
                    0x28 => Opcode::JR(Some(Condition::Z), SignedImm8),
                    0x38 => Opcode::JR(Some(Condition::C), SignedImm8),
                    0x09 | 0x19 | 0x29 | 0x39 => Opcode::ADDHL(Reg16::HL, reg16_arithmetic),
                    0x0A | 0x1A => Opcode::LDARegMem(Reg8::A, Mem(reg16_arithmetic)),
                    0x2A => Opcode::LDAHLMemInc(Reg8::A, Mem(Reg16::HL)),
                    0x3A => Opcode::LDAHLMemDec(Reg8::A, Mem(Reg16::HL)),
                    0x0B | 0x1B | 0x2B | 0x3B => Opcode::DEC16(reg16_arithmetic),
                    0x0C | 0x1C | 0x2C | 0x3C => Opcode::INC(dst.left().unwrap()),
                    0x0D | 0x1D | 0x2D | 0x3D => Opcode::DEC(dst.left().unwrap()),
                    0x0E | 0x1E | 0x2E | 0x3E => Opcode::LDRegImm(dst.left().unwrap(), Imm8),
                    0x0F => Opcode::RRCA,
                    0x1F => Opcode::RRA,
                    0x2F => Opcode::CPL,
                    0x3F => Opcode::CCF,
                    0x40..=0x7F => {
                        if value == 0x76 {
                            Opcode::HALT
                        } else if value % 8 == 6 {
                            Opcode::LDRegHLMem(dst.left().unwrap(), src.right().unwrap())
                        } else if (0x70..=0x77).contains(&value) {
                            Opcode::LDHLMemReg(dst.right().unwrap(), src.left().unwrap())
                        } else {
                            Opcode::LD(dst.left().unwrap(), src.left().unwrap())
                        }
                    }
                    0x80..=0x85 | 0x87 => Opcode::ADD(Reg8::A, src.left().unwrap()),
                    0x86 => Opcode::ADDHLMem(Reg8::A, src.right().unwrap()),
                    0x88..=0x8D | 0x8F => Opcode::ADC(Reg8::A, src.left().unwrap()),
                    0x8E => Opcode::ADCHLMem(Reg8::A, src.right().unwrap()),
                    0x90..=0x95 | 0x97 => Opcode::SUB(Reg8::A, src.left().unwrap()),
                    0x96 => Opcode::SUBHLMem(Reg8::A, src.right().unwrap()),
                    0x98..=0x9D | 0x9F => Opcode::SBC(Reg8::A, src.left().unwrap()),
                    0x9E => Opcode::SBCHLMem(Reg8::A, src.right().unwrap()),
                    0xA0..=0xA5 | 0xA7 => Opcode::AND(Reg8::A, src.left().unwrap()),
                    0xA6 => Opcode::ANDHLMem(Reg8::A, src.right().unwrap()),
                    0xA8..=0xAD | 0xAF => Opcode::XOR(Reg8::A, src.left().unwrap()),
                    0xAE => Opcode::XORHLMem(Reg8::A, src.right().unwrap()),
                    0xB0..=0xB5 | 0xB7 => Opcode::OR(Reg8::A, src.left().unwrap()),
                    0xB6 => Opcode::ORHLMem(Reg8::A, src.right().unwrap()),
                    0xB8..=0xBD | 0xBF => Opcode::CP(Reg8::A, src.left().unwrap()),
                    0xBE => Opcode::CPHLMem(Reg8::A, src.right().unwrap()),
                    0xC0 => Opcode::RET(Some(Condition::NZ)),
                    0xD0 => Opcode::RET(Some(Condition::NC)),
                    0xE0 => Opcode::LDHImmMemA(IOMem(Imm8), Reg8::A),
                    0xF0 => Opcode::LDHAImmMem(Reg8::A, IOMem(Imm8)),
                    0xC1 | 0xD1 | 0xE1 | 0xF1 => Opcode::POP(reg16_stack),
                    0xC2 => Opcode::JP(Some(Condition::NZ), Imm16),
                    0xD2 => Opcode::JP(Some(Condition::NC), Imm16),
                    0xE2 => Opcode::LDHCMemA(IOMem(Reg8::C), Reg8::A),
                    0xF2 => Opcode::LDHACMem(Reg8::A, IOMem(Reg8::C)),
                    0xC3 => Opcode::JP(None, Imm16),
                    0xD3 | 0xE3 => Opcode::Illegal,
                    0xF3 => Opcode::DI,
                    0xC4 => Opcode::CALL(Some(Condition::NZ), Imm16),
                    0xD4 => Opcode::CALL(Some(Condition::NC), Imm16),
                    0xE4 | 0xF4 => Opcode::Illegal,
                    0xC5 | 0xD5 | 0xE5 | 0xF5 => Opcode::PUSH(reg16_stack),
                    0xC6 => Opcode::ADDImm(Reg8::A, Imm8),
                    0xD6 => Opcode::SUBImm(Reg8::A, Imm8),
                    0xE6 => Opcode::ANDImm(Reg8::A, Imm8),
                    0xF6 => Opcode::ORImm(Reg8::A, Imm8),
                    0xC7 | 0xD7 | 0xE7 | 0xF7 => Opcode::RST(rst_vector),
                    0xC8 => Opcode::RET(Some(Condition::Z)),
                    0xD8 => Opcode::RET(Some(Condition::C)),
                    0xE8 => Opcode::ADDSP(Reg16::SP, SignedImm8),
                    0xF8 => Opcode::LDHLSPOffset(SignedImm8),
                    0xC9 => Opcode::RET(None),
                    0xD9 => Opcode::RETI,
                    0xE9 => Opcode::JPHL,
                    0xF9 => Opcode::LDSPHL,
                    0xCA => Opcode::JP(Some(Condition::Z), Imm16),
                    0xDA => Opcode::JP(Some(Condition::C), Imm16),
                    0xEA => Opcode::LDImmMemA(Mem(Imm16), Reg8::A),
                    0xFA => Opcode::LDAImmMem(Reg8::A, Mem(Imm16)),
                    0xCB => Decoder::parse_cb(cpu, pc + 1),
                    0xDB | 0xEB => Opcode::Illegal,
                    0xFB => Opcode::EI,
                    0xCC => Opcode::CALL(Some(Condition::Z), Imm16),
                    0xDC => Opcode::CALL(Some(Condition::C), Imm16),
                    0xEC | 0xFC => Opcode::Illegal,
                    0xCD => Opcode::CALL(None, Imm16),
                    0xDD | 0xED | 0xFD => Opcode::Illegal,
                    0xCE => Opcode::ADCImm(Reg8::A, Imm8),
                    0xDE => Opcode::SBCImm(Reg8::A, Imm8),
                    0xEE => Opcode::XORImm(Reg8::A, Imm8),
                    0xFE => Opcode::CPImm(Reg8::A, Imm8),
                    0xCF | 0xDF | 0xEF | 0xFF => Opcode::RST(rst_vector),
                })
            }
        }
    }
}

/// The different instructions available for the CPU.
#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    // Special (Pink)
    NOP,
    STOP,
    HALT,
    DI,
    EI,

    // Jumps, calls, returns (Orange)
    JR(Option<Condition>, SignedImm8),
    RET(Option<Condition>),
    JP(Option<Condition>, Imm16),
    CALL(Option<Condition>, Imm16),
    RETI,
    JPHL,
    RST(u8),

    // 16-bit operations (Salmon)
    INC16(Reg16),
    DEC16(Reg16),
    ADDHL(Reg16, Reg16),
    ADDSP(Reg16, SignedImm8),

    // 16-bit loads & stack operations (Green)
    LD16(Reg16, Imm16),
    LDImmMemSP(Mem<Imm16>, Reg16), // 0x08
    POP(Reg16),
    PUSH(Reg16),
    LDHLSPOffset(SignedImm8),
    LDSPHL,

    // Loads (Blue)
    LDARegMem(Reg8, Mem<Reg16>),
    LDHLMemIncA(Mem<Reg16>, Reg8),
    LDHLMemDecA(Mem<Reg16>, Reg8),
    LDRegMemA(Mem<Reg16>, Reg8),
    LDAHLMemInc(Reg8, Mem<Reg16>),
    LDAHLMemDec(Reg8, Mem<Reg16>),
    LDRegImm(Reg8, Imm8),
    LDHLMemImm(Mem<Reg16>, Imm8),
    LD(Reg8, Reg8),
    LDRegHLMem(Reg8, Mem<Reg16>),
    LDHLMemReg(Mem<Reg16>, Reg8),
    LDHImmMemA(IOMem<Imm8>, Reg8),
    LDHAImmMem(Reg8, IOMem<Imm8>),
    LDHCMemA(IOMem<Reg8>, Reg8),
    LDHACMem(Reg8, IOMem<Reg8>),
    LDImmMemA(Mem<Imm16>, Reg8),
    LDAImmMem(Reg8, Mem<Imm16>),

    // Arithmetic operations (Yellow)
    INC(Reg8),
    INCHLMem(Mem<Reg16>),
    DEC(Reg8),
    DECHLMem(Mem<Reg16>),
    DAA,
    SCF,
    CPL,
    CCF,
    ADD(Reg8, Reg8),
    ADDHLMem(Reg8, Mem<Reg16>),
    ADDImm(Reg8, Imm8),
    ADC(Reg8, Reg8),
    ADCHLMem(Reg8, Mem<Reg16>),
    ADCImm(Reg8, Imm8),
    SUB(Reg8, Reg8),
    SUBHLMem(Reg8, Mem<Reg16>),
    SUBImm(Reg8, Imm8),
    SBC(Reg8, Reg8),
    SBCHLMem(Reg8, Mem<Reg16>),
    SBCImm(Reg8, Imm8),
    AND(Reg8, Reg8),
    ANDHLMem(Reg8, Mem<Reg16>),
    ANDImm(Reg8, Imm8),
    XOR(Reg8, Reg8),
    XORHLMem(Reg8, Mem<Reg16>),
    XORImm(Reg8, Imm8),
    OR(Reg8, Reg8),
    ORHLMem(Reg8, Mem<Reg16>),
    ORImm(Reg8, Imm8),
    CP(Reg8, Reg8),
    CPHLMem(Reg8, Mem<Reg16>),
    CPImm(Reg8, Imm8),

    // Bit operations (Cyan)
    RLCA,
    RLA,
    RRCA,
    RRA,
    RLC(Reg8),
    RLCMem(Mem<Reg16>),
    RRC(Reg8),
    RRCMem(Mem<Reg16>),
    RL(Reg8),
    RLMem(Mem<Reg16>),
    RR(Reg8),
    RRMem(Mem<Reg16>),
    SLA(Reg8),
    SLAMem(Mem<Reg16>),
    SRA(Reg8),
    SRAMem(Mem<Reg16>),
    SWAP(Reg8),
    SWAPMem(Mem<Reg16>),
    SRL(Reg8),
    SRLMem(Mem<Reg16>),
    BIT(u8, Reg8),
    BITMem(u8, Mem<Reg16>),
    RES(u8, Reg8),
    RESMem(u8, Mem<Reg16>),
    SET(u8, Reg8),
    SETMem(u8, Mem<Reg16>),

    // Illegal
    Illegal,
}

impl Opcode {
    /// Returns the number of cycles an instruction takes. If there is a
    /// condition associated to the instruction, both the "action taken" count
    /// and the "action not taken" count are returned.
    pub fn cycles(&self) -> (usize, Option<usize>) {
        match self {
            Opcode::CALL(c, _) => match c {
                Some(_) => (6, Some(3)),
                None => (6, None),
            },
            Opcode::RET(c) => match c {
                Some(_) => (5, Some(2)),
                None => (4, None),
            },
            Opcode::LDImmMemSP(_, _) => (5, None),
            Opcode::JP(c, _) => match c {
                Some(_) => (4, Some(3)),
                None => (4, None),
            },
            Opcode::PUSH(_)
            | Opcode::RST(_)
            | Opcode::ADDSP(_, _)
            | Opcode::RETI
            | Opcode::LDImmMemA(_, _)
            | Opcode::RLCMem(_)
            | Opcode::RRCMem(_)
            | Opcode::RLMem(_)
            | Opcode::RRMem(_)
            | Opcode::SLAMem(_)
            | Opcode::SRAMem(_)
            | Opcode::SWAPMem(_)
            | Opcode::SRLMem(_)
            | Opcode::BITMem(_, _)
            | Opcode::SETMem(_, _)
            | Opcode::RESMem(_, _)
            | Opcode::LDAImmMem(_, _) => (4, None),
            Opcode::JR(c, _) => match c {
                Some(_) => (3, Some(2)),
                None => (3, None),
            },
            Opcode::LD16(_, _)
            | Opcode::INCHLMem(_)
            | Opcode::DECHLMem(_)
            | Opcode::LDHLMemImm(_, _)
            | Opcode::LDHImmMemA(_, _)
            | Opcode::LDHAImmMem(_, _)
            | Opcode::POP(_)
            | Opcode::LDHLSPOffset(_) => (3, None),
            Opcode::LDRegMemA(_, _)
            | Opcode::LDHLMemDecA(_, _)
            | Opcode::LDHLMemIncA(_, _)
            | Opcode::INC16(_)
            | Opcode::LDRegImm(_, _)
            | Opcode::ADDHL(_, _)
            | Opcode::LDARegMem(_, _)
            | Opcode::LDAHLMemDec(_, _)
            | Opcode::LDAHLMemInc(_, _)
            | Opcode::DEC16(_)
            | Opcode::LDRegHLMem(_, _)
            | Opcode::LDHLMemReg(_, _)
            | Opcode::ADDHLMem(_, _)
            | Opcode::ADCHLMem(_, _)
            | Opcode::SUBHLMem(_, _)
            | Opcode::SBCHLMem(_, _)
            | Opcode::ANDHLMem(_, _)
            | Opcode::XORHLMem(_, _)
            | Opcode::ORHLMem(_, _)
            | Opcode::CPHLMem(_, _)
            | Opcode::ADDImm(_, _)
            | Opcode::ADCImm(_, _)
            | Opcode::SUBImm(_, _)
            | Opcode::SBCImm(_, _)
            | Opcode::ANDImm(_, _)
            | Opcode::XORImm(_, _)
            | Opcode::ORImm(_, _)
            | Opcode::CPImm(_, _)
            | Opcode::LDHCMemA(_, _)
            | Opcode::LDHACMem(_, _)
            | Opcode::RLC(_)
            | Opcode::RRC(_)
            | Opcode::RL(_)
            | Opcode::RR(_)
            | Opcode::SLA(_)
            | Opcode::SRA(_)
            | Opcode::SWAP(_)
            | Opcode::SRL(_)
            | Opcode::BIT(_, _)
            | Opcode::SET(_, _)
            | Opcode::RES(_, _)
            | Opcode::LDSPHL => (2, None),
            _ => (1, None),
        }
    }

    /// Returns the size of the instruction, in bytes.
    pub fn size(&self) -> usize {
        match self {
            Opcode::LD16(_, _)
            | Opcode::LDImmMemSP(_, _)
            | Opcode::JP(_, _)
            | Opcode::CALL(_, _)
            | Opcode::LDImmMemA(_, _)
            | Opcode::LDAImmMem(_, _) => 3,
            Opcode::STOP
            | Opcode::JR(_, _)
            | Opcode::LDRegImm(_, _)
            | Opcode::LDHLMemImm(_, _)
            | Opcode::ADDImm(_, _)
            | Opcode::ADCImm(_, _)
            | Opcode::SUBImm(_, _)
            | Opcode::SBCImm(_, _)
            | Opcode::ANDImm(_, _)
            | Opcode::XORImm(_, _)
            | Opcode::ORImm(_, _)
            | Opcode::CPImm(_, _)
            | Opcode::LDHImmMemA(_, _)
            | Opcode::LDHAImmMem(_, _)
            | Opcode::ADDSP(_, _)
            | Opcode::RLC(_)
            | Opcode::RLCMem(_)
            | Opcode::RRC(_)
            | Opcode::RRCMem(_)
            | Opcode::RL(_)
            | Opcode::RLMem(_)
            | Opcode::RR(_)
            | Opcode::RRMem(_)
            | Opcode::SLA(_)
            | Opcode::SLAMem(_)
            | Opcode::SRA(_)
            | Opcode::SRAMem(_)
            | Opcode::SWAP(_)
            | Opcode::SWAPMem(_)
            | Opcode::SRL(_)
            | Opcode::SRLMem(_)
            | Opcode::BIT(_, _)
            | Opcode::BITMem(_, _)
            | Opcode::SET(_, _)
            | Opcode::SETMem(_, _)
            | Opcode::RES(_, _)
            | Opcode::RESMem(_, _)
            | Opcode::LDHLSPOffset(_) => 2,
            _ => 1,
        }
    }

    pub fn try_display(&self, cpu: &CPU, pc: Option<u16>) -> Result<String, String> {
        let pc = pc.unwrap_or_else(|| Reg16::PC.read(cpu));
        let unknown = || String::from("??");

        Mem(pc + 1)
            .try_read(cpu)
            .ok()
            .ok_or_else(unknown)
            .map(|i| i as i8)
            .and_then(|signed_imm8| {
                Mem(pc + 1)
                    .try_read(cpu)
                    .ok()
                    .ok_or_else(unknown)
                    .and_then(|imm8| {
                        Mem16(pc + 1)
                            .try_read(cpu)
                            .ok()
                            .ok_or_else(unknown)
                            .map(|imm16| match self {
                                Opcode::NOP => String::from("NOP"),
                                Opcode::STOP => String::from("STOP 0"),
                                Opcode::HALT => String::from("HALT"),
                                Opcode::DI => String::from("DI"),
                                Opcode::EI => String::from("EI"),

                                Opcode::JR(c, _) => match c {
                                    Some(c) => format!("JR {}, %{:+}", c, signed_imm8),
                                    None => format!("JR %{:+}", signed_imm8),
                                },
                                Opcode::RET(c) => match c {
                                    Some(c) => format!("RET {}", c),
                                    None => String::from("RET"),
                                },
                                Opcode::JP(c, _) => match c {
                                    Some(c) => format!("JP {}, $0x{:04x}", c, imm16),
                                    None => format!("JP $0x{:04x}", imm16),
                                },
                                Opcode::CALL(c, _) => match c {
                                    Some(c) => format!("CALL {}, $0x{:04x}", c, imm16),
                                    None => format!("CALL $0x{:04x}", imm16),
                                },
                                Opcode::RETI => String::from("RETI"),
                                Opcode::JPHL => String::from("JP (HL)"),
                                Opcode::RST(v) => format!("RST 0x{:02x}", v),

                                Opcode::INC16(r) => format!("INC {}", r),
                                Opcode::DEC16(r) => format!("DEC {}", r),
                                Opcode::ADDHL(dst, src) => format!("ADD {}, {}", dst, src),
                                Opcode::ADDSP(dst, _) => format!("ADD {}, %{:+}", dst, signed_imm8),

                                Opcode::LD16(r, _) => format!("LD {}, %0x{:04x}", r, imm16),
                                Opcode::LDImmMemSP(_, r) => format!("LD (0x{:04x}), {}", imm16, r),
                                Opcode::POP(r) => format!("POP {}", r),
                                Opcode::PUSH(r) => format!("PUSH {}", r),
                                Opcode::LDHLSPOffset(_) => {
                                    format!("LD HL, SP+0x{:02x}", signed_imm8)
                                }
                                Opcode::LDSPHL => String::from("LD SP, HL"),

                                Opcode::LDARegMem(r, m) => format!("LD {}, {}", r, m),
                                Opcode::LDHLMemIncA(m, r) => format!("LD ({}+), {}", m.0, r),
                                Opcode::LDHLMemDecA(m, r) => format!("LD ({}-), {}", m.0, r),
                                Opcode::LDRegMemA(m, r) => format!("LD {}, {}", r, m),
                                Opcode::LDAHLMemInc(r, m) => format!("LD {}, ({}+)", r, m.0),
                                Opcode::LDAHLMemDec(r, m) => format!("LD {}, ({}-)", r, m.0),
                                Opcode::LDRegImm(r, _) => format!("LD {}, %0x{:02x}", r, imm8),
                                Opcode::LDHLMemImm(r, _) => format!("LD {}, %0x{:02x}", r, imm8),
                                Opcode::LD(d, s) => format!("LD {}, {}", d, s),
                                Opcode::LDRegHLMem(r, m) => format!("LD {}, {}", r, m),
                                Opcode::LDHLMemReg(m, r) => format!("LD {}, {}", m, r),
                                Opcode::LDHImmMemA(_, r) => {
                                    format!("LDH ($FF00+0x{:02x}), {}", imm8, r)
                                }
                                Opcode::LDHAImmMem(r, _) => {
                                    format!("LDH {}, ($FF00+0x{:02x})", r, imm8)
                                }
                                Opcode::LDHCMemA(_, r) => format!("LDH ($FF00+C), {}", r),
                                Opcode::LDHACMem(r, _) => format!("LDH {}, ($FF00+C)", r),
                                Opcode::LDImmMemA(_, r) => format!("LD ($0x{:04x}), {}", imm16, r),
                                Opcode::LDAImmMem(r, _) => format!("LD {}, ($0x{:04x})", r, imm16),

                                Opcode::INC(r) => format!("INC {}", r),
                                Opcode::INCHLMem(m) => format!("INC {}", m),
                                Opcode::DEC(r) => format!("DEC {}", r),
                                Opcode::DECHLMem(m) => format!("DEC {}", m),
                                Opcode::DAA => String::from("DAA"),
                                Opcode::SCF => String::from("SCF"),
                                Opcode::CPL => String::from("CPL"),
                                Opcode::CCF => String::from("CCF"),
                                Opcode::ADD(d, s) => format!("ADD {}, {}", d, s),
                                Opcode::ADDHLMem(r, m) => format!("ADD {}, {}", r, m),
                                Opcode::ADDImm(r, _) => format!("ADD {}, %0x{:02x}", r, imm16),
                                Opcode::SUB(d, s) => format!("SUB {}, {}", d, s),
                                Opcode::SUBHLMem(r, m) => format!("SUB {}, {}", r, m),
                                Opcode::SUBImm(r, _) => format!("SUB {}, %0x{:02x}", r, imm16),
                                Opcode::ADC(d, s) => format!("ADC {}, {}", d, s),
                                Opcode::ADCHLMem(r, m) => format!("ADC {}, {}", r, m),
                                Opcode::ADCImm(r, _) => format!("ADC {}, %0x{:02x}", r, imm16),
                                Opcode::SBC(d, s) => format!("SBC {}, {}", d, s),
                                Opcode::SBCHLMem(r, m) => format!("SBC {}, {}", r, m),
                                Opcode::SBCImm(r, _) => format!("SBC {}, %0x{:02x}", r, imm16),
                                Opcode::AND(d, s) => format!("AND {}, {}", d, s),
                                Opcode::ANDHLMem(r, m) => format!("AND {}, {}", r, m),
                                Opcode::ANDImm(r, _) => format!("AND {}, %0x{:02x}", r, imm16),
                                Opcode::XOR(d, s) => format!("XOR {}, {}", d, s),
                                Opcode::XORHLMem(r, m) => format!("XOR {}, {}", r, m),
                                Opcode::XORImm(r, _) => format!("XOR {}, %0x{:02x}", r, imm16),
                                Opcode::OR(d, s) => format!("OR {}, {}", d, s),
                                Opcode::ORHLMem(r, m) => format!("OR {}, {}", r, m),
                                Opcode::ORImm(r, _) => format!("OR {}, %0x{:02x}", r, imm16),
                                Opcode::CP(d, s) => format!("CP {}, {}", d, s),
                                Opcode::CPHLMem(r, m) => format!("CP {}, {}", r, m),
                                Opcode::CPImm(r, _) => format!("CP {}, %0x{:02x}", r, imm16),

                                Opcode::RLCA => String::from("RLCA"),
                                Opcode::RLA => String::from("RLA"),
                                Opcode::RRCA => String::from("RRCA"),
                                Opcode::RRA => String::from("RRA"),
                                Opcode::RLC(d) => format!("RLC {}", d),
                                Opcode::RLCMem(d) => format!("RLC {}", d),
                                Opcode::RRC(d) => format!("RRC {}", d),
                                Opcode::RRCMem(d) => format!("RRC {}", d),
                                Opcode::RL(d) => format!("RL {}", d),
                                Opcode::RLMem(d) => format!("RL {}", d),
                                Opcode::RR(d) => format!("RR {}", d),
                                Opcode::RRMem(d) => format!("RR {}", d),
                                Opcode::SLA(d) => format!("SLA {}", d),
                                Opcode::SLAMem(d) => format!("SLA {}", d),
                                Opcode::SRA(d) => format!("SRA {}", d),
                                Opcode::SRAMem(d) => format!("SRA {}", d),
                                Opcode::SWAP(d) => format!("SWAP {}", d),
                                Opcode::SWAPMem(d) => format!("SWAP {}", d),
                                Opcode::SRL(d) => format!("SRL {}", d),
                                Opcode::SRLMem(d) => format!("SRL {}", d),
                                Opcode::BIT(b, d) => format!("BIT {}, {}", b, d),
                                Opcode::BITMem(b, d) => format!("BIT {}, {}", b, d),
                                Opcode::SET(b, d) => format!("SET {}, {}", b, d),
                                Opcode::SETMem(b, d) => format!("SET {}, {}", b, d),
                                Opcode::RES(b, d) => format!("RES {}, {}", b, d),
                                Opcode::RESMem(b, d) => format!("RES {}, {}", b, d),

                                Opcode::Illegal => String::from("<illegal opcode>"),
                            })
                    })
            })
    }

    /// Returns a `String` representation of the instruction, using the
    /// provided address to interpret the context. If the provided address
    /// is `None`, the current PC register is used as an address.
    pub fn display(&self, cpu: &CPU, pc: Option<u16>) -> String {
        self.try_display(cpu, pc).ok().unwrap()
    }
}
