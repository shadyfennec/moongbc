use crate::cartridge::MBCError;
use std::fmt;
use std::iter::repeat;

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

/// Describes a location of memory that can be read and written to,
/// provided an address.
pub trait MemoryRegion {
    /// Tries to read a byte in the memory at the specified memory location.
    /// If the read fails (for example, an out of bounds, or an access to VRAM
    /// in-between frames), it returns `None`.
    fn read(&self, addr: u16) -> Result<u8, MemoryError>;
    /// Writes a byte to the provided memory location.
    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError>;
}

/// Represents the VRAM of the GBC.
#[derive(Default)]
pub struct VRAM {
    data: Vec<u8>,
    bank: usize,
}

impl VRAM {
    pub fn new() -> VRAM {
        VRAM {
            data: repeat(0).take(1024 * 16).collect(),
            bank: 0,
        }
    }
}

impl MemoryRegion for VRAM {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        let index = (self.bank * 1024 * 8) + (addr as usize - 0x8000);
        self.data
            .get(index)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        let index = (self.bank * 1024 * 8) + (addr as usize - 0x8000);
        if let Some(loc) = self.data.get_mut(index) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}

/// Describes the working RAM of the GBC
#[derive(Default)]
pub struct WRAM {
    data: Vec<u8>,
    bank: usize,
}

impl WRAM {
    pub fn new() -> WRAM {
        WRAM {
            data: repeat(0).take(1024 * 32).collect(),
            bank: 1,
        }
    }

    pub fn set_bank(&mut self, bank: usize) {
        assert!((1..8).contains(&bank));
        self.bank = bank;
    }
}

impl MemoryRegion for WRAM {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        let index = match addr {
            0xC000..=0xCFFF => addr as usize - 0xC000,
            _ => (self.bank * 1024 * 4) + (addr as usize - 0xC000),
        };

        self.data
            .get(index)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        let index = match addr {
            0xC000..=0xCFFF => addr as usize - 0xC000,
            _ => (self.bank * 1024 * 4) + (addr as usize - 0xC000),
        };

        if let Some(loc) = self.data.get_mut(index) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}

/// Describes the Object Attribute Memory for the GBC
#[derive(Default)]
pub struct OAM {
    data: Vec<u8>,
}

impl OAM {
    pub fn new() -> OAM {
        OAM {
            data: repeat(0).take(0xA0).collect(),
        }
    }
}

impl MemoryRegion for OAM {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        self.data
            .get(addr as usize - 0xFE00)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFE00) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}

/// Describes the High RAM of the GBC
#[derive(Default)]
pub struct HRAM {
    data: Vec<u8>,
}

impl HRAM {
    pub fn new() -> HRAM {
        HRAM {
            data: repeat(0).take(0x7F).collect(),
        }
    }
}

impl MemoryRegion for HRAM {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        self.data
            .get(addr as usize - 0xFF80)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFF80) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}

#[derive(Default)]
pub struct UnusedRegion {
    data: Vec<u8>,
}

impl UnusedRegion {
    pub fn new() -> UnusedRegion {
        UnusedRegion {
            data: repeat(0).take(0x60).collect(),
        }
    }
}

impl MemoryRegion for UnusedRegion {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        self.data
            .get(addr as usize - 0xFEA0)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFEA0) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}

#[derive(Default)]
pub struct IORegisters {
    data: Vec<u8>,
}

impl IORegisters {
    pub fn new() -> Self {
        Self {
            data: repeat(0).take(0x80).collect(),
        }
    }
}

impl MemoryRegion for IORegisters {
    fn read(&self, addr: u16) -> Result<u8, MemoryError> {
        self.data
            .get(addr as usize - 0xFF00)
            .copied()
            .ok_or(MemoryError(MemoryErrorKind::OutOfBounds))
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), MemoryError> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFF00) {
            *loc = value;
            Ok(())
        } else {
            Err(MemoryError(MemoryErrorKind::OutOfBounds))
        }
    }
}
