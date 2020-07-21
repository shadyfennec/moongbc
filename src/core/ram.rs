use std::iter::repeat;

/// Describes a location of memory that can be read and written to,
/// provided an address.
pub trait RAM {
    /// Tries to read a byte in the memory at the specified memory location.
    /// If the read fails (for example, an out of bounds, or an access to VRAM
    /// in-between frames), it returns `None`.
    fn read(&self, addr: u16) -> Option<u8>;
    /// Writes a byte to the provided memory location.
    fn write(&mut self, addr: u16, value: u8) -> Result<(), String>;
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

impl RAM for VRAM {
    fn read(&self, addr: u16) -> Option<u8> {
        let index = (self.bank * 1024 * 8) + (addr as usize - 0x8000);
        self.data.get(index).copied()
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), String> {
        let index = (self.bank * 1024 * 8) + (addr as usize - 0x8000);
        if let Some(loc) = self.data.get_mut(index) {
            *loc = value;
            Ok(())
        } else {
            Err("Out of bounds".to_string())
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
}

impl RAM for WRAM {
    fn read(&self, addr: u16) -> Option<u8> {
        let index = match addr {
            0xC000..=0xCFFF => addr as usize - 0xC000,
            _ => (self.bank * 1024 * 4) + (addr as usize - 0xC000),
        };

        self.data.get(index).copied()
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), String> {
        let index = match addr {
            0xC000..=0xCFFF => addr as usize - 0xC000,
            _ => (self.bank * 1024 * 4) + (addr as usize - 0xC000),
        };

        if let Some(loc) = self.data.get_mut(index) {
            *loc = value;
            Ok(())
        } else {
            Err("Out of bounds".to_string())
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

impl RAM for OAM {
    fn read(&self, addr: u16) -> Option<u8> {
        self.data.get(addr as usize - 0xFE00).copied()
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), String> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFE00) {
            *loc = value;
            Ok(())
        } else {
            Err("Out of bounds".to_string())
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

impl RAM for HRAM {
    fn read(&self, addr: u16) -> Option<u8> {
        self.data.get(addr as usize - 0xFF80).copied()
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), String> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFF80) {
            *loc = value;
            Ok(())
        } else {
            Err("Out of bounds".to_string())
        }
    }
}

/// Describes the IO registers of the GBC
#[derive(Default)]
pub struct IORegisters {
    data: Vec<u8>,
}

impl IORegisters {
    pub fn new() -> IORegisters {
        IORegisters {
            data: repeat(0).take(0x80).collect(),
        }
    }
}

impl RAM for IORegisters {
    fn read(&self, addr: u16) -> Option<u8> {
        self.data.get(addr as usize - 0xFF00).copied()
    }

    fn write(&mut self, addr: u16, value: u8) -> Result<(), String> {
        if let Some(loc) = self.data.get_mut(addr as usize - 0xFF00) {
            *loc = value;
            Ok(())
        } else {
            Err("Out of bounds".to_string())
        }
    }
}
