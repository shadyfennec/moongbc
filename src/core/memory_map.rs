use crate::cartridge::Cartridge;
use crate::cpu::CPU;
use crate::ram::{
    IORegisters, MemoryError, MemoryErrorKind, MemoryRegion, UnusedRegion, HRAM, OAM, VRAM, WRAM,
};
use crate::{register::Reg16, BitField, Dst, ReadWriteError, ReadWriteErrorKind, Src};

use std::fmt;

/// A location in memory, indexed by a 16-bit value.
#[derive(Debug, Copy, Clone)]
pub struct Mem<S: Src<u16>>(pub S);

impl<S: Src<u16>> Src<u8> for Mem<S> {
    fn try_read(&self, cpu: &CPU) -> Result<u8, ReadWriteError> {
        cpu.memory_map
            .read(self.0.read(cpu))
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u16>> Dst<u8> for Mem<S> {
    fn try_write(&self, cpu: &mut CPU, value: u8) -> Result<(), ReadWriteError> {
        cpu.memory_map
            .write(self.0.read(cpu), value)
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
/// between `$0xFF00` and `$0xFF7F`. It is indexed by a single
/// 8-bit value.
#[derive(Debug, Copy, Clone)]
pub struct IOMem<S: Src<u8>>(pub S);

impl<S: Src<u8>> Src<u8> for IOMem<S> {
    fn try_read(&self, cpu: &CPU) -> Result<u8, ReadWriteError> {
        let offset = (self.0.read(cpu) as i8) as i32;
        let base = 0xFF00i32;

        let addr = (base + offset) as u16;
        cpu.memory_map
            .read(addr)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
    }
}

impl<S: Src<u8>> Dst<u8> for IOMem<S> {
    fn try_write(&self, cpu: &mut CPU, value: u8) -> Result<(), ReadWriteError> {
        let offset = (self.0.read(cpu) as i8) as i32;
        let base = 0xFF00i32;

        let addr = (base + offset) as u16;
        cpu.memory_map
            .write(addr, value)
            .map_err(|e| ReadWriteError(ReadWriteErrorKind::MemoryError(e)))
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

/// Describes the memory map, responsible for dispatching
/// any address in the address space to the correct location
/// in the system.
#[derive(Default)]
pub struct MemoryMap {
    cartridge: Option<Cartridge>,
    bootstrap: BootstrapRom,
    boot_flag: bool,
    vram: VRAM,
    wram: WRAM,
    oam: OAM,
    hram: HRAM,
    unused_region: UnusedRegion,
    io_registers: IORegisters,
    interrupt_enable: BitField,
}

impl MemoryMap {
    /// Creates a new MemoryMap, which in turns initializes the other memory
    /// locations.
    pub fn new() -> MemoryMap {
        MemoryMap {
            cartridge: None,
            bootstrap: BootstrapRom::new(),
            boot_flag: true,
            vram: VRAM::new(),
            wram: WRAM::new(),
            oam: OAM::new(),
            hram: HRAM::new(),
            unused_region: UnusedRegion::new(),
            io_registers: IORegisters::new(),
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
            0x0000..=0x7FFF | 0xA000..=0xBFFF => {
                if self.boot_flag && addr < self.bootstrap.len() as u16 {
                    "Boot"
                } else {
                    "ROM "
                }
            }
            0x8000..=0x9FFF => "VRAM",
            0xC000..=0xDFFF => "WRAM",
            0xE000..=0xFDFF => "WRAM",
            0xFE00..=0xFE9F => "OAM ",
            0xFEA0..=0xFEFF => "N/A ",
            0xFF00..=0xFF7F => "IO  ",
            0xFF80..=0xFFFE => "HRAM",
            0xFFFF => "INT ",
        }
    }

    fn read_register(&self, addr: u16) -> Result<u8, MemoryError> {
        match addr {
            _ => self.io_registers.read(addr),
        }
    }

    fn write_register(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        match addr {
            0xFF70 => {
                self.wram.set_bank((value & 7) as usize);
                Ok(())
            }
            _ => self.io_registers.write(addr, value),
        }
    }
}

impl MemoryRegion for MemoryMap {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        match addr {
            0x0000..=0x7FFF | 0xA000..=0xBFFF => {
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
            0x8000..=0x9FFF => self.vram.read(addr),
            0xC000..=0xDFFF => self.wram.read(addr),
            0xE000..=0xFDFF => self.wram.read(addr - 0x2000),
            0xFE00..=0xFE9F => self.oam.read(addr),
            0xFEA0..=0xFEFF => self.unused_region.read(addr),
            0xFF00..=0xFF7F => self.read_register(addr),
            0xFF80..=0xFFFE => self.hram.read(addr),
            0xFFFF => Ok(self.interrupt_enable.into()),
        }
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        match addr {
            0x0000..=0x7FFF | 0xA000..=0xBFFF => self
                .cartridge
                .as_mut()
                .unwrap()
                .write(addr, value)
                .map_err(|e| MemoryError(MemoryErrorKind::MBCError(e))),
            0x8000..=0x9FFF => self.vram.write(addr, value),
            0xC000..=0xDFFF => self.wram.write(addr, value),
            0xE000..=0xFDFF => self.wram.write(addr - 0x2000, value),
            0xFE00..=0xFE9F => self.oam.write(addr, value),
            0xFEA0..=0xFEFF => self.unused_region.write(addr, value),
            0xFF00..=0xFF7F => self.write_register(addr, value),
            0xFF80..=0xFFFE => self.hram.write(addr, value),
            0xFFFF => {
                self.interrupt_enable.write(value);
                Ok(())
            }
        }
    }
}
