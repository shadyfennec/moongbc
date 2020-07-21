use std::fs::File;
use std::io::Read;
use std::path::Path;

/// The type of a Cartridge. The cartridges usually differ in terms of
/// external RAM, ROM size, and Memory Bank Controllers. The cartridge type
/// is specified in the ROM header at address `$0x0147`.
#[derive(Debug)]
pub enum CartridgeType {
    ROMOnly,
    MBC1,
    MBC1RAM,
    MBC1RAMBattery,
    MBC2,
    MBC2B,
    ROMRAM,
    ROMRAMBattery,
    MMM01,
    MMM01RAM,
    MMM01RAMBattery,
    MBC3TimerBattery,
    MBC3TimerRAMBattery,
    MBC3,
    MBC3RAM,
    MBC3RamBattery,
    MBC5,
    MBC5RAM,
    MBC5RAMBattery,
    MBC5Rumble,
    MBC5RumbleRAM,
    MBC5RumbleRAMBattery,
    MBC6,
    MBC7SensorRumbleRamBattery,
    PocketCamera,
    BandaiTama5,
    HuC3,
    HuC1RAMBattery,
    Unknown,
}

impl From<u8> for CartridgeType {
    fn from(value: u8) -> Self {
        match value {
            0x00 => CartridgeType::ROMOnly,
            0x01 => CartridgeType::MBC1,
            0x02 => CartridgeType::MBC1RAM,
            0x03 => CartridgeType::MBC1RAMBattery,
            0x05 => CartridgeType::MBC2,
            0x06 => CartridgeType::MBC2B,
            0x08 => CartridgeType::ROMRAM,
            0x09 => CartridgeType::ROMRAMBattery,
            0x0B => CartridgeType::MMM01,
            0x0C => CartridgeType::MMM01RAM,
            0x0D => CartridgeType::MMM01RAMBattery,
            0x0F => CartridgeType::MBC3TimerBattery,
            0x10 => CartridgeType::MBC3TimerRAMBattery,
            0x11 => CartridgeType::MBC3,
            0x12 => CartridgeType::MBC3RAM,
            0x13 => CartridgeType::MBC3RamBattery,
            0x19 => CartridgeType::MBC5,
            0x1A => CartridgeType::MBC5RAM,
            0x1B => CartridgeType::MBC5RAMBattery,
            0x1C => CartridgeType::MBC5Rumble,
            0x1D => CartridgeType::MBC5RumbleRAM,
            0x1E => CartridgeType::MBC5RumbleRAMBattery,
            0x20 => CartridgeType::MBC6,
            0x22 => CartridgeType::MBC7SensorRumbleRamBattery,
            0xFC => CartridgeType::PocketCamera,
            0xFD => CartridgeType::BandaiTama5,
            0xFE => CartridgeType::HuC3,
            0xFF => CartridgeType::Unknown,
            _ => CartridgeType::HuC1RAMBattery,
        }
    }
}

/// Describes an error that could happen during a read or write in the cartridge
/// memory map.
#[derive(Debug)]
pub enum MBCError {
    /// A read or write was attempted on memory region `0xA000` - `0xBFFF`, but
    /// no external RAM is present in the cartridge.
    NoRAM,

    /// A read or write was attempted on memory region `0xA000` - `0xBFFF`, but
    /// the external RAM is currently disabled.
    RAMDisabled,

    /// A write was issued on a read-only location (usually, a portion of ROM which
    /// has no functional purpose other than reading ROM).
    ReadOnly,

    /// A read or write was issued at an address that isn't covered by the MBC:
    /// any address outside of `0x0000` - `0x7FFF` and `0xA000` - `0xBFFF` will
    /// produce this error.
    InvalidAddress,

    /// A ROM read was issued while the ROM bank number is invalid for the
    /// current MBC: for example, trying to read a byte in bank 2 in a ROM-only
    /// cartridge, which has only 2 banks directly mapped to `0x0000` - `0x7FFF`.
    InvalidROMBank,

    /// An external RAM read or write was issued while the RAM bank number is invalid (too high
    /// for the current amount of external RAM).
    InvalidRAMBank,
}

/// A read operation performed on the cartridge, parsed by the MBC to produce a single
/// instruction on either the external RAM or ROM with an offset computed from the initial
/// read address and the current ROM/RAM bank number.
pub enum MBCRead {
    ReadROM(usize),
    ReadRAM(usize),
}

/// A write instruction performed on the cartridge, parsed by the MBC to either produce
/// a write operation on RAM, with an offset computed from the address and the current RAM
/// bank number, or no operation performed in the case of an internal operation (such as bank
/// or mode switching).
pub enum MBCWrite {
    WriteRAM(usize, u8),
    Nothing,
}

type MBCResult<T> = Result<T, MBCError>;

/// Describes a Memory Bank Controller, a small piece of hardware responsible of handling read
/// and writes to the mapped ROM/external RAM memory locations. It handles special operations,
/// such as bank switching, and translates memory-mapped addresses to real ROM or RAM addresses.
pub trait MemoryBankController {
    /// Read a byte from the cartridge's memory map. This can either succeed, producing a `MBCRead`
    /// operation, or fail, with a descriptive `MBCError` variant.
    fn read(&self, addr: u16) -> MBCResult<MBCRead>;

    /// Write a byte into the cartridge's memory map; this handles special operations such as
    /// bank switching. The operation can succeed, production a `MBCWrite` operation, or fail,
    /// producing a descriptive `MBCError` variant.
    fn write(&mut self, addr: u16, value: u8) -> MBCResult<MBCWrite>;
}

/// A plain ROM with potential RAM and battery, corresponding to codes `0x00`, `0x08` and `0x09`
/// in the cartridge header's type field.
pub struct PlainMBC {
    ram_enable: Option<bool>,
}

impl PlainMBC {
    pub fn new(ram: bool) -> PlainMBC {
        let ram_enable = if ram { Some(false) } else { None };
        PlainMBC { ram_enable }
    }
}

impl MemoryBankController for PlainMBC {
    fn read(&self, addr: u16) -> MBCResult<MBCRead> {
        match addr {
            0x0000..=0x7FFF => Ok(MBCRead::ReadROM(addr as _)),
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        Ok(MBCRead::ReadRAM(addr as usize - 0xA000))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            _ => Err(MBCError::InvalidAddress),
        }
    }

    fn write(&mut self, addr: u16, value: u8) -> MBCResult<MBCWrite> {
        match addr {
            0x0000..=0x1FFF => match self.ram_enable {
                Some(_) => {
                    if value != 0 {
                        self.ram_enable = Some(true)
                    } else {
                        self.ram_enable = Some(false)
                    }
                    Ok(MBCWrite::Nothing)
                }
                None => Err(MBCError::NoRAM),
            },
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        Ok(MBCWrite::WriteRAM(addr as usize - 0xA000, value))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            0x2000..=0x9FFF => Err(MBCError::ReadOnly),
            _ => Err(MBCError::InvalidAddress),
        }
    }
}

/// Describes the current mode of certain types of MBC, which can only handle
/// either higher-bits for ROM banks, or RAM banks.
enum ROMRAMMode {
    ROMBankingMode,
    RAMBankingMode,
}

/// The MBC1, corresponding to the codes `0x01`, `0x02` and `0x03` in the cartridge header's
/// type field.
pub struct MBC1 {
    mode: ROMRAMMode,
    current_rom_bank: u8,
    current_ram_bank: u8,
    ram_enable: Option<bool>,
}

impl MBC1 {
    /// Creates a new MBC1 controller, specifying the presence or absence of RAM.
    pub fn new(ram: bool) -> MBC1 {
        let ram_enable = if ram { Some(false) } else { None };
        MBC1 {
            mode: ROMRAMMode::ROMBankingMode,
            current_rom_bank: 0,
            current_ram_bank: 0,
            ram_enable,
        }
    }
}

impl MemoryBankController for MBC1 {
    fn read(&self, addr: u16) -> MBCResult<MBCRead> {
        match addr {
            0x0000..=0x3FFF => Ok(MBCRead::ReadROM(addr as _)),
            0x4000..=0x7FFF => {
                let offset = (self.current_rom_bank as usize) * 1024 * 16;

                Ok(MBCRead::ReadROM(offset + (addr as usize - 0x4000)))
            }
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        let offset = (self.current_ram_bank as usize) * 1024 * 8;

                        Ok(MBCRead::ReadRAM(offset + (addr as usize - 0xA000)))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            _ => Err(MBCError::InvalidAddress),
        }
    }

    fn write(&mut self, addr: u16, value: u8) -> MBCResult<MBCWrite> {
        match addr {
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        let offset = (self.current_ram_bank as usize) * 1024 * 8;

                        Ok(MBCWrite::WriteRAM(offset + (addr as usize - 0xA000), value))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            0x0000..=0x1FFF => match self.ram_enable {
                Some(_) => {
                    self.ram_enable = match value & 0xA {
                        0x0A => Some(true),
                        _ => Some(false),
                    };
                    Ok(MBCWrite::Nothing)
                }
                None => Err(MBCError::NoRAM),
            },
            0x2000..=0x3FFF => {
                let bank_number = match value {
                    0x00 | 0x20 | 0x40 | 0x60 => value + 1,
                    _ => value,
                } & 0b0001_1111;
                self.current_rom_bank |= bank_number;

                Ok(MBCWrite::Nothing)
            }
            0x4000..=0x5FFF => match self.mode {
                ROMRAMMode::RAMBankingMode => match self.ram_enable {
                    Some(_) => {
                        self.current_ram_bank = value & 3;
                        Ok(MBCWrite::Nothing)
                    }
                    None => Err(MBCError::NoRAM),
                },
                ROMRAMMode::ROMBankingMode => {
                    let high_rom_bank = (value & 3) << 5;
                    self.current_rom_bank |= high_rom_bank;
                    Ok(MBCWrite::Nothing)
                }
            },
            0x6000..=0x7FFF => match value & 1 {
                0 => {
                    self.mode = ROMRAMMode::ROMBankingMode;
                    Ok(MBCWrite::Nothing)
                }
                _ => match self.ram_enable {
                    Some(_) => {
                        self.mode = ROMRAMMode::RAMBankingMode;
                        Ok(MBCWrite::Nothing)
                    }
                    None => Err(MBCError::NoRAM),
                },
            },
            _ => Err(MBCError::InvalidAddress),
        }
    }
}

/// The MBC5, corresponding to codes `0x1A`, `0x1B`, `0x1C`, `0x1D` and `0x1E` in the cartridge
/// header's type field.
pub struct MBC5 {
    current_rom_bank: u16,
    current_ram_bank: u8,
    ram_enable: Option<bool>,
}

impl MBC5 {
    pub fn new(ram: bool) -> MBC5 {
        let ram_enable = if ram { Some(false) } else { None };
        MBC5 {
            current_rom_bank: 0,
            current_ram_bank: 0,
            ram_enable,
        }
    }
}

impl MemoryBankController for MBC5 {
    fn read(&self, addr: u16) -> MBCResult<MBCRead> {
        match addr {
            0x0000..=0x3FFF => Ok(MBCRead::ReadROM(addr as _)),
            0x4000..=0x7FFF => {
                let offset = (self.current_rom_bank as usize) * 1024 * 16;

                Ok(MBCRead::ReadROM(offset + (addr as usize - 0x4000)))
            }
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        let offset = (self.current_ram_bank as usize) * 1024 * 8;

                        Ok(MBCRead::ReadRAM(offset + (addr as usize - 0xA000)))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            _ => Err(MBCError::InvalidAddress),
        }
    }

    fn write(&mut self, addr: u16, value: u8) -> MBCResult<MBCWrite> {
        match addr {
            0xA000..=0xBFFF => match self.ram_enable {
                Some(b) => {
                    if b {
                        let offset = (self.current_ram_bank as usize) * 1024 * 8;

                        Ok(MBCWrite::WriteRAM(offset + (addr as usize - 0xA000), value))
                    } else {
                        Err(MBCError::RAMDisabled)
                    }
                }
                None => Err(MBCError::NoRAM),
            },
            0x0000..=0x1FFF => match self.ram_enable {
                Some(_) => {
                    self.ram_enable = match value & 0xA {
                        0x0A => Some(true),
                        _ => Some(false),
                    };
                    Ok(MBCWrite::Nothing)
                }
                None => Err(MBCError::NoRAM),
            },
            0x2000..=0x2FFF => {
                self.current_rom_bank |= value as u16;

                Ok(MBCWrite::Nothing)
            }
            0x3000..=0x3FFF => {
                self.current_rom_bank |= (value as u16 & 1) << 8;

                Ok(MBCWrite::Nothing)
            }
            0x4000..=0x5FFF => match self.ram_enable {
                Some(_) => {
                    self.current_ram_bank = value & 4;
                    Ok(MBCWrite::Nothing)
                }
                None => Err(MBCError::NoRAM),
            },
            _ => Err(MBCError::InvalidAddress),
        }
    }
}

/// A cartridge, containing ROM (and potentially RAM) for a GBC game.
pub struct Cartridge {
    pub(crate) sgb_flag: bool,
    pub(crate) mbc: Box<dyn MemoryBankController>,
    pub(crate) rom: Vec<u8>,
    pub(crate) ram: Option<Vec<u8>>,
}

impl Cartridge {
    /// Loads a cartridge from its ROM file.
    pub fn load<P: AsRef<Path>>(path: P) -> std::io::Result<Cartridge> {
        let mut file = File::open(path)?;
        let mut rom = Vec::new();
        file.read_to_end(&mut rom)?;

        let sgb_flag = match rom[0x0146] {
            0x03 => true,
            _ => false,
        };

        let cartridge_type: CartridgeType = rom[0x0147].into();

        let mbc: Box<dyn MemoryBankController> = match cartridge_type {
            CartridgeType::ROMOnly => Box::new(PlainMBC::new(false)),
            CartridgeType::ROMRAM | CartridgeType::ROMRAMBattery => Box::new(PlainMBC::new(true)),
            CartridgeType::MBC1 => Box::new(MBC1::new(false)),
            CartridgeType::MBC1RAM | CartridgeType::MBC1RAMBattery => Box::new(MBC1::new(true)),
            CartridgeType::MBC5 | CartridgeType::MBC5Rumble => Box::new(MBC5::new(false)),
            CartridgeType::MBC5RAM
            | CartridgeType::MBC5RAMBattery
            | CartridgeType::MBC5RumbleRAM
            | CartridgeType::MBC5RumbleRAMBattery => Box::new(MBC5::new(true)),
            _ => panic!(format!("Unsupported MBC: {:?}", cartridge_type)),
        };

        let ram = match rom[0x0149] {
            0x00 => None,
            0x01 => Some(2),
            0x02 => Some(8),
            0x03 => Some(32),
            0x04 => Some(128),
            0x05 => Some(64),
            _ => None,
        }
        .map(|e| std::iter::repeat(0).take(1024 * e).collect::<Vec<u8>>());

        Ok(Cartridge {
            sgb_flag,
            mbc,
            rom,
            ram,
        })
    }

    /// Read a byte from the cartridge's memory map.
    pub fn read(&self, addr: u16) -> MBCResult<u8> {
        match self.mbc.read(addr) {
            Ok(op) => match op {
                MBCRead::ReadRAM(addr) => match self.ram.as_ref() {
                    Some(ram) => match ram.get(addr) {
                        Some(v) => Ok(*v),
                        None => Err(MBCError::InvalidRAMBank),
                    },
                    None => Err(MBCError::NoRAM),
                },
                MBCRead::ReadROM(addr) => match self.rom.get(addr) {
                    Some(v) => Ok(*v),
                    None => Err(MBCError::InvalidROMBank),
                },
            },
            Err(e) => Err(e),
        }
    }

    /// Write a byte in the cartridge's memory map.
    pub fn write(&mut self, addr: u16, value: u8) -> MBCResult<()> {
        match self.mbc.write(addr, value) {
            Ok(op) => match op {
                MBCWrite::Nothing => Ok(()),
                MBCWrite::WriteRAM(addr, value) => match self.ram.as_mut() {
                    Some(ram) => match ram.get_mut(addr) {
                        Some(v) => {
                            *v = value;
                            Ok(())
                        }
                        None => Err(MBCError::InvalidRAMBank),
                    },
                    None => Err(MBCError::NoRAM),
                },
            },
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod cartridge_tests {
    use super::*;

    #[test]
    fn load_cartridge() {
        let _ = Cartridge::load("pkmn_yellow.gbc").unwrap();
    }

    #[test]
    fn mbc5_read_bank_0() {
        let cartridge = Cartridge::load("pkmn_yellow.gbc").unwrap();

        let target = String::from("POKEMON YELLOW");
        let mut buf = Vec::new();

        for addr in 0x0134..=0x141 {
            let value = cartridge.read(addr).unwrap() as char;
            buf.push(value);
        }

        assert_eq!(target, buf.iter().collect::<String>())
    }
}
