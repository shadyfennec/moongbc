/*
*  The memory map of the CGB, spread out to a 16-bit address space,
*  is the following:
*
*  | Start | End  | Description                     |
*  |-------+------+---------------------------------|
*  | 0000  | 3FFF | 16kB ROM bank 00                |
*  | 4000  | 7FFF | 16kB ROM bank 01-NN             |
*  | 8000  | 9FFF | 8kB Video RAM (VRAM)            |
*  | A000  | BFFF | 8kB External RAM                |
*  | C000  | CFFF | 4kB Work RAM (WRAM) bank 0      |
*  | D000  | DFFF | 4kB Work RAM (WRAM) bank 1-7    |
*  | E000  | FDFF | Mirror of C000-DDFF (ECHO RAM)  |
*  | FE00  | FE9F | Sprite Attribute Table (OAM)    |
*  | FEA0  | FEFF | Not usable                      |
*  | FF00  | FF7F | I/O registers                   |
*  | FF80  | FFFE | High RAM (HRAM)                 |
*  | FFFF  | FFFF | Interrupts Enable Register (IE) |
*
*  source: https://gbdev.gg8.se/wiki/articles/Memory_Map
*/

use std::{fmt, path::Path};

use crate::{cartridge::Cartridge, gpu::GPU};
use crate::{
    cartridge::MBCError,
    register::{Reg8, Registers},
};
use crate::{register::Reg16, BitField};

pub const ROM_START: u16 = 0x0000;
pub const ROM_END: u16 = 0x7FFF;
pub const CARTRIDGE_HEADER_START: u16 = 0x0100;
pub const CARTRIDGE_HEADER_END: u16 = 0x01FF;

pub const VRAM_START: u16 = 0x8000;
pub const VRAM_END: u16 = 0x9FFF;
pub const VRAM_SIZE: usize = (VRAM_END - VRAM_START + 1) as usize;

pub const ERAM_START: u16 = 0xA000;
pub const ERAM_END: u16 = 0xBFFF;

pub const WRAM_START: u16 = 0xC000;
pub const WRAM_END: u16 = 0xDFFF;
pub const WRAM_BANK_0_START: u16 = WRAM_START;
pub const WRAM_BANK_0_END: u16 = 0xCFFF;
pub const WRAM_BANK_N_START: u16 = 0xD000;
pub const WRAM_BANK_N_END: u16 = WRAM_END;
pub const WRAM_SIZE: usize = (WRAM_END - WRAM_START + 1) as usize;

pub const ECHO_START: u16 = 0xE000;
pub const ECHO_END: u16 = 0xFDFF;
pub const ECHO_SIZE: usize = (ECHO_END - ECHO_START + 1) as usize;

pub const OAM_START: u16 = 0xFE00;
pub const OAM_END: u16 = 0xFE9F;
pub const OAM_SIZE: usize = (OAM_END - OAM_START + 1) as usize;

pub const UNUSED_START: u16 = 0xFEA0;
pub const UNUSED_END: u16 = 0xFEFF;

pub const IO_START: u16 = 0xFF00;
pub const IO_END: u16 = 0xFF7F;
pub const IO_SIZE: usize = (IO_END - IO_START + 1) as usize;

pub const HRAM_START: u16 = 0xFF80;
pub const HRAM_END: u16 = 0xFFFE;
pub const HRAM_SIZE: usize = (HRAM_END - HRAM_START + 1) as usize;

pub const EI_ADDRESS: u16 = 0xFFFF;
#[derive(Debug, Copy, Clone)]
pub enum MemIdx {
    Imm16,
    Imm8,
    Reg16(Reg16),
    Reg8(Reg8),
    Direct(u16),
}

impl fmt::Display for MemIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemIdx::Imm16 => write!(f, "(Imm16)"),
            MemIdx::Imm8 => write!(f, "(0x{:04x} + Imm8)", IO_START),
            MemIdx::Reg16(r) => write!(f, "({})", r),
            MemIdx::Reg8(r) => write!(f, "(0x{:04x} + {}", IO_START, r),
            MemIdx::Direct(addr) => write!(f, "(0x{:04x})", addr),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Mem(pub MemIdx);

impl Mem {
    pub fn read(&self, memory: &Interconnect, registers: &Registers) -> u8 {
        let addr: u16 = match self.0 {
            MemIdx::Imm16 => Imm16.read(memory, registers),
            MemIdx::Imm8 => IO_START + (Imm8.read(memory, registers) as u16),
            MemIdx::Reg16(r) => r.read(registers),
            MemIdx::Reg8(r) => IO_START + r.read(registers) as u16,
            MemIdx::Direct(addr) => addr,
        };
        memory.read(addr)
    }

    pub fn write(&self, memory: &mut Interconnect, registers: &Registers, value: u8) {
        let addr = match self.0 {
            MemIdx::Imm16 => Imm16.read(memory, registers),
            MemIdx::Imm8 => IO_START + Imm8.read(memory, registers) as u16,
            MemIdx::Reg16(r) => r.read(registers),
            MemIdx::Reg8(r) => IO_START + r.read(registers) as u16,
            MemIdx::Direct(addr) => addr,
        };
        memory.write(addr, value);
    }
}

impl fmt::Display for Mem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MemIdx::Imm16 => write!(f, "(Imm16)"),
            MemIdx::Imm8 => write!(f, "(0x{:04x} + Imm8)", IO_START),
            MemIdx::Reg16(r) => write!(f, "({})", r),
            MemIdx::Reg8(r) => write!(f, "(0x{:04x} + {}", IO_START, r),
            MemIdx::Direct(addr) => write!(f, "(0x{:04x})", addr),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Mem16(pub MemIdx);

impl Mem16 {
    pub fn read(&self, memory: &Interconnect, registers: &Registers) -> u16 {
        let addr = match self.0 {
            MemIdx::Imm16 => Imm16.read(memory, registers),
            MemIdx::Imm8 => IO_START + Imm8.read(memory, registers) as u16,
            MemIdx::Reg16(r) => r.read(registers),
            MemIdx::Reg8(r) => IO_START + r.read(registers) as u16,
            MemIdx::Direct(addr) => addr,
        };

        let low = memory.read(addr);
        let high = memory.read(addr + 1);
        (high as u16) << 8 | (low as u16)
    }

    pub fn write(&self, memory: &mut Interconnect, registers: &Registers, value: u16) {
        let addr = match self.0 {
            MemIdx::Imm16 => Imm16.read(memory, registers),
            MemIdx::Imm8 => IO_START + Imm8.read(memory, registers) as u16,
            MemIdx::Reg16(r) => r.read(registers),
            MemIdx::Reg8(r) => IO_START + r.read(registers) as u16,
            MemIdx::Direct(addr) => addr,
        };
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        memory.write(addr, low);
        memory.write(addr + 1, high);
    }
}

/// An immediate value, usually represented by a byte after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct Imm8;

impl Imm8 {
    pub fn read(&self, memory: &Interconnect, registers: &Registers) -> u8 {
        memory.read(Reg16::PC.read(registers) + 1)
    }
}

// An immediate 16-bit value, usually represented by two bytes after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct Imm16;

impl Imm16 {
    pub fn read(&self, memory: &Interconnect, registers: &Registers) -> u16 {
        let pc = Reg16::PC.read(registers);
        let low = memory.read(pc + 1);
        let high = memory.read(pc + 2);

        ((high as u16) << 8) | (low as u16)
    }
}

/// A signed immediate value, usually represented by two bytes after an instruction.
#[derive(Debug, Copy, Clone)]
pub struct SignedImm8;

impl SignedImm8 {
    pub fn read(&self, memory: &Interconnect, registers: &Registers) -> i8 {
        memory.read(Reg16::PC.read(registers) + 1) as i8
    }
}

/// Represents the ROM present in any GBC console regardless of the
/// cartridge inserted (if any). It contains instructions to initialize
/// memory and display the bootup sequence (the Nintendo logo, for example).
/// At startup, the cartridge ROM isn't available, as this boot ROM occupies
/// the memory map range usually reserved for ROM bank 0.
/// It can be disabled by writing to `xFF50`, in which case the cartridge ROM
/// can be accessed.
#[derive(Default)]
struct BootstrapRom {
    data: Vec<u8>,
}

impl BootstrapRom {
    pub fn new() -> BootstrapRom {
        BootstrapRom {
            data: include_bytes!("bootstrap.bin").to_vec(),
        }
    }

    pub fn read(&self, addr: u16) -> Option<u8> {
        self.data.get(addr as usize).copied()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

pub struct WRAM {
    data: [u8; (WRAM_SIZE * 8) as usize],
    bank: usize,
}

impl Default for WRAM {
    fn default() -> Self {
        WRAM::new()
    }
}

impl WRAM {
    pub fn new() -> Self {
        WRAM {
            data: [0; WRAM_SIZE * 8],
            bank: 1,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        let offset = self.bank * WRAM_SIZE;
        match addr {
            WRAM_BANK_0_START..=WRAM_BANK_0_END => self.data[(addr - WRAM_START) as usize],
            WRAM_BANK_N_START..=WRAM_BANK_N_END => {
                self.data[((addr - WRAM_START) as usize) + offset]
            }
            ECHO_START..=ECHO_END => self.read(addr - WRAM_SIZE as u16),
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        let offset = self.bank * WRAM_SIZE;
        match addr {
            WRAM_BANK_0_START..=WRAM_BANK_0_END => self.data[(addr - WRAM_START) as usize] = value,
            WRAM_BANK_N_START..=WRAM_BANK_N_END => {
                self.data[((addr - WRAM_START) as usize) + offset] = value
            }
            ECHO_START..=ECHO_END => self.write(addr - WRAM_SIZE as u16, value),
            _ => unreachable!(),
        }
    }

    pub fn set_bank(&mut self, bank: usize) {
        assert!((0..8).contains(&bank));
        self.bank = bank.max(1);
    }
}

pub struct HRAM {
    data: [u8; HRAM_SIZE],
}

impl Default for HRAM {
    fn default() -> Self {
        HRAM::new()
    }
}

impl HRAM {
    pub fn new() -> Self {
        HRAM {
            data: [0; HRAM_SIZE],
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.data[(addr - HRAM_START) as usize]
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.data[(addr - HRAM_START) as usize] = value;
    }
}

#[derive(Debug)]
/// Represents an error that can occur during a memory operation
pub struct MemoryError(pub MemoryErrorKind);

#[derive(Debug)]
/// The different kinds of errors that can occur during a memory operation
pub enum MemoryErrorKind {
    /// An error occured in the MBC
    MBC(MBCError),
    /// The address specified is out of bounds
    OutOfBounds,
}

impl fmt::Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MemoryErrorKind::MBC(e) => write!(f, "MBC error: {}", e),
            MemoryErrorKind::OutOfBounds => write!(f, "Address out of bounds"),
        }
    }
}

/// Describes the memory map, responsible for dispatching
/// any address in the address space to the correct location
/// in the system.
pub struct Interconnect {
    cartridge: Option<Cartridge>,
    bootstrap: BootstrapRom,
    boot_flag: bool,
    gpu: GPU,
    pub(crate) wram: WRAM,
    hram: HRAM,
    io_registers: [u8; IO_SIZE],
    interrupt_enable: BitField,
}

impl Default for Interconnect {
    fn default() -> Self {
        Interconnect::new()
    }
}

impl Interconnect {
    /// Creates a new MemoryMap, which in turns initializes the other memory
    /// locations.
    pub fn new() -> Interconnect {
        Interconnect {
            cartridge: None,
            bootstrap: BootstrapRom::new(),
            boot_flag: true,
            gpu: GPU::new(),
            wram: WRAM::new(),
            hram: HRAM::new(),
            io_registers: [0; IO_SIZE],
            interrupt_enable: BitField::from(0),
        }
    }

    /// Sets the cartridge ROM, making it available for IO.
    pub fn set_cartridge<P: AsRef<Path>>(&mut self, path: P) {
        self.cartridge = Some(Cartridge::load(path).unwrap());
    }

    /// Returns the name of the location pointed to by an address.
    pub fn location(&self, addr: u16) -> &'static str {
        match addr {
            ROM_START..=ROM_END => "ROM",
            VRAM_START..=VRAM_END => "VRAM",
            ERAM_START..=ERAM_END => "ERAM",
            WRAM_START..=WRAM_END => "WRAM",
            ECHO_START..=ECHO_END => "ECHO",
            OAM_START..=OAM_END => "OAM ",
            UNUSED_START..=UNUSED_END => "N/A ",
            IO_START..=IO_END => "IO  ",
            HRAM_START..=HRAM_END => "HRAM",
            EI_ADDRESS => "INT ",
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            ROM_START..=0x00FF | 0x0200..=ROM_END | ERAM_START..=ERAM_END => {
                if self.boot_flag && addr < self.bootstrap.len() as u16 {
                    self.bootstrap.read(addr).unwrap()
                } else {
                    self.cartridge.as_ref().unwrap().read(addr).unwrap()
                }
            }
            CARTRIDGE_HEADER_START..=CARTRIDGE_HEADER_END => {
                self.cartridge.as_ref().unwrap().read(addr).unwrap()
            }
            VRAM_START..=VRAM_END => self.gpu.read(addr),
            WRAM_START..=WRAM_END => self.wram.read(addr),
            ECHO_START..=ECHO_END => self.wram.read(addr),
            OAM_START..=OAM_END => self.gpu.read(addr),
            UNUSED_START..=UNUSED_END => 0,
            IO_START..=IO_END => self.io_registers[(addr - IO_START) as usize],
            HRAM_START..=HRAM_END => self.hram.read(addr),
            EI_ADDRESS => self.interrupt_enable.into(),
        }
    }

    pub fn try_read(&self, addr: u16) -> Result<u8, MemoryError> {
        match addr {
            ROM_START..=0x00FF | 0x0200..=ROM_END | ERAM_START..=ERAM_END => {
                if self.boot_flag && addr < self.bootstrap.len() as u16 {
                    self.bootstrap
                        .read(addr)
                        .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
                } else {
                    self.cartridge
                        .as_ref()
                        .unwrap()
                        .read(addr)
                        .map_err(|e| MemoryError(MemoryErrorKind::MBC(e)))
                }
            }
            CARTRIDGE_HEADER_START..=CARTRIDGE_HEADER_END => self
                .cartridge
                .as_ref()
                .unwrap()
                .read(addr)
                .map_err(|e| MemoryError(MemoryErrorKind::MBC(e))),

            VRAM_START..=VRAM_END => Ok(self.gpu.read(addr)),
            WRAM_START..=WRAM_END => Ok(self.wram.read(addr)),
            ECHO_START..=ECHO_END => Ok(self.wram.read(addr)),
            OAM_START..=OAM_END => Ok(self.gpu.read(addr)),
            UNUSED_START..=UNUSED_END => Ok(0),
            IO_START..=IO_END => Ok(self.io_registers[(addr - IO_START) as usize]),
            HRAM_START..=HRAM_END => Ok(self.hram.read(addr)),
            EI_ADDRESS => Ok(self.interrupt_enable.into()),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            ROM_START..=ROM_END | ERAM_START..=ERAM_END => {
                self.cartridge.as_mut().unwrap().write(addr, value).unwrap()
            }
            VRAM_START..=VRAM_END => self.gpu.write(addr, value),
            WRAM_START..=WRAM_END | ECHO_START..=ECHO_END => self.wram.write(addr, value),
            OAM_START..=OAM_END => {
                self.gpu.write(addr, value);
            }
            UNUSED_START..=UNUSED_END => {}
            IO_START..=IO_END => self.io_registers[(addr - IO_START) as usize] = value,
            HRAM_START..=HRAM_END => self.hram.write(addr, value),
            EI_ADDRESS => self.interrupt_enable.write(value),
        }
    }
}
