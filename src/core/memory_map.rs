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

use crate::cpu::CPU;
use crate::{
    cartridge::{Cartridge, MBCError},
    gpu::GPU,
};

use crate::{register::Reg16, BitField, Dst, ReadWriteError, ReadWriteErrorKind, Src};

use std::fmt;

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

/// A location in memory, indexed by a 16-bit value.
#[derive(Debug, Copy, Clone)]
pub struct Mem<S: Src<u16>>(pub S);

impl<S: Src<u16>> Src<u8> for Mem<S> {
    fn try_read(&self, cpu: &CPU) -> Result<u8, ReadWriteError> {
        let addr = self.0.read(cpu);
        cpu.handle_io_read(addr);
        cpu.memory_map
            .read(addr)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u16>> Dst<u8> for Mem<S> {
    fn try_write(&self, cpu: &mut CPU, value: u8) -> Result<(), ReadWriteError> {
        let addr = self.0.read(cpu);
        cpu.handle_io_write(addr, value);
        cpu.memory_map
            .write(addr, value)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u16>> Dst<u16> for Mem<S> {
    fn try_write(&self, cpu: &mut CPU, value: u16) -> Result<(), ReadWriteError> {
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        cpu.memory_map
            .write(self.0.read(cpu), low)
            .and_then(|_| cpu.memory_map.write(self.0.read(cpu) + 1, high))
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl fmt::Display for Mem<u16> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(0x{:04x})", self.0)
    }
}

impl fmt::Display for Mem<Reg16> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.0)
    }
}

/// A special location in memory, able to retrieve a 16-bit value
pub struct Mem16<S: Src<u16>>(pub S);

impl<S: Src<u16>> Src<u16> for Mem16<S> {
    fn try_read(&self, cpu: &CPU) -> Result<u16, ReadWriteError> {
        /*
        let low = cpu.memory_map.read(self.0.read(cpu)).map(|v| v as u16);
        let high = cpu.memory_map.read(self.0.read(cpu) + 1).map(|v| v as u16);

        match (low, high) {
            (Ok(low), Ok(high)) => Ok((high << 8) | low),
            _ => None,
        }
        */
        cpu.memory_map
            .read(self.0.read(cpu))
            .map(|v| v as u16)
            .and_then(|low| {
                cpu.memory_map
                    .read(self.0.read(cpu) + 1)
                    .map(|high| (high as u16) << 8 | low)
            })
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u16>> Dst<u16> for Mem16<S> {
    fn try_write(&self, cpu: &mut CPU, value: u16) -> Result<(), ReadWriteError> {
        let high = (value >> 8) as u8;
        let low = (value & 0xFF) as u8;

        cpu.memory_map
            .write(self.0.read(cpu), low)
            .and_then(|_| cpu.memory_map.write(self.0.read(cpu) + 1, high))
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

/// Describes a memory location inside the IO registers range,
/// between `xFF00` and `xFF7F`. It is indexed by a single
/// 8-bit value.
#[derive(Debug, Copy, Clone)]
pub struct IOMem<S: Src<u8>>(pub S);

impl<S: Src<u8>> Src<u8> for IOMem<S> {
    fn try_read(&self, cpu: &CPU) -> Result<u8, ReadWriteError> {
        let offset = self.0.read(cpu);
        let base = IO_START;

        let addr = base + offset as u16;
        cpu.handle_io_read(addr);
        cpu.memory_map
            .read(addr)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u8>> Dst<u8> for IOMem<S> {
    fn try_write(&self, cpu: &mut CPU, value: u8) -> Result<(), ReadWriteError> {
        let offset = self.0.read(cpu);
        let base = IO_START;

        let addr = base + offset as u16;
        cpu.handle_io_write(addr, value);
        cpu.memory_map
            .write(addr, value)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

#[derive(Debug)]
/// Represents an error that can occur during a memory operation
pub struct MemoryError(pub MemoryErrorKind);

#[derive(Debug)]
/// The different kinds of errors that can occur during a memory operation
pub enum MemoryErrorKind {
    /// An error occured in the MBC
    MBCError(MBCError),
    /// The address specified is out of bounds
    OutOfBounds,
    /// A write operation was issued on a read-only memory region
    ReadOnly,
    /// Special error used for the unused data range `0xFEA0` to `0xFEFF`
    UnusedRange,
}

impl fmt::Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MemoryErrorKind::MBCError(e) => write!(f, "MBC error: {}", e),
            MemoryErrorKind::OutOfBounds => write!(f, "Address out of bounds"),
            MemoryErrorKind::ReadOnly => write!(f, "Write issued on read-only address"),
            MemoryErrorKind::UnusedRange => write!(f, "Unused memory region"),
        }
    }
}

/// Represents the ROM present in any GBC console regardless of the
/// cartridge inserted (if any). It contains instructions to initialize
/// memory and display the bootup sequence (the Nintendo logo, for example).
/// At startup, the cartridge ROM isn't available, as this boot ROM occupies
/// the memory map range usually reserved for ROM bank 0.
/// It can be disabled by writing to `$0xFF50`, in which case the cartridge ROM
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

/// Describes the memory map, responsible for dispatching
/// any address in the address space to the correct location
/// in the system.
pub struct MemoryMap {
    cartridge: Option<Cartridge>,
    bootstrap: BootstrapRom,
    boot_flag: bool,
    gpu: GPU,
    pub(crate) wram: WRAM,
    hram: HRAM,
    io_registers: [u8; IO_SIZE],
    interrupt_enable: BitField,
}

impl Default for MemoryMap {
    fn default() -> Self {
        MemoryMap::new()
    }
}

impl MemoryMap {
    /// Creates a new MemoryMap, which in turns initializes the other memory
    /// locations.
    pub fn new() -> MemoryMap {
        MemoryMap {
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
    pub fn set_cartridge(&mut self, cartridge: Cartridge) {
        self.cartridge = Some(cartridge);
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

    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
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
                        .map_err(|e| MemoryError(MemoryErrorKind::MBCError(e)))
                }
            }
            CARTRIDGE_HEADER_START..=CARTRIDGE_HEADER_END => self
                .cartridge
                .as_ref()
                .unwrap()
                .read(addr)
                .map_err(|e| MemoryError(MemoryErrorKind::MBCError(e))),
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

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        match addr {
            ROM_START..=ROM_END | ERAM_START..=ERAM_END => self
                .cartridge
                .as_mut()
                .unwrap()
                .write(addr, value)
                .map_err(|e| MemoryError(MemoryErrorKind::MBCError(e))),
            VRAM_START..=VRAM_END => {
                self.gpu.write(addr, value);
                Ok(())
            }
            WRAM_START..=WRAM_END | ECHO_START..=ECHO_END => {
                self.wram.write(addr, value);
                Ok(())
            }
            OAM_START..=OAM_END => {
                self.gpu.write(addr, value);
                Ok(())
            }
            UNUSED_START..=UNUSED_END => Ok(()),
            IO_START..=IO_END => {
                self.io_registers[(addr - IO_START) as usize] = value;
                Ok(())
            }
            HRAM_START..=HRAM_END => {
                self.hram.write(addr, value);
                Ok(())
            }
            EI_ADDRESS => {
                self.interrupt_enable.write(value);
                Ok(())
            }
        }
    }
}
