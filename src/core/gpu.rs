pub use crate::memory_map::{OAM_END, OAM_SIZE, OAM_START, VRAM_END, VRAM_SIZE, VRAM_START};

pub struct GPU {
    vram: [u8; VRAM_SIZE],
    oam: [u8; OAM_SIZE],
}

impl Default for GPU {
    fn default() -> Self {
        GPU::new()
    }
}

impl GPU {
    pub fn new() -> Self {
        GPU {
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            VRAM_START..=VRAM_END => self.vram[(addr - VRAM_START) as usize],
            OAM_START..=OAM_END => self.oam[(addr - OAM_START) as usize],
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            VRAM_START..=VRAM_END => self.vram[(addr - VRAM_START) as usize] = value,
            OAM_START..=OAM_END => self.oam[(addr - OAM_START) as usize] = value,
            _ => unreachable!(),
        }
    }
}
