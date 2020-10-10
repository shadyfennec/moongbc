pub use crate::memory_map::{OAM_END, OAM_SIZE, OAM_START, VRAM_END, VRAM_SIZE, VRAM_START};
use crate::{
    gui::GUI,
    memory_map::{Interconnect, Interrupt},
    BitField,
};

pub const WIDTH: usize = 144;
pub const HEIGHT: usize = 160;

/// A color as represented in the Gameboy, with values going from 0x00 to 0x1F
#[derive(Copy, Clone)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

const COLOR_WHITE: Color = Color {
    r: 0x1F,
    g: 0x1F,
    b: 0x1F,
};

impl From<(u8, u8, u8)> for Color {
    fn from(v: (u8, u8, u8)) -> Self {
        Color {
            r: v.0,
            g: v.1,
            b: v.2,
        }
    }
}

impl From<Color> for u32 {
    fn from(color: Color) -> Self {
        let r = (color.r as u32) << 16;
        let g = (color.g as u32) << 8;
        let b = color.b as u32;

        r | g | b
    }
}
#[derive(Copy, Clone)]
pub enum GPUMode {
    HBlank,
    VBlank,
    OAM,
    VRAM,
}

impl GPUMode {
    pub fn bits(&self) -> (bool, bool) {
        match self {
            GPUMode::HBlank => (false, false),
            GPUMode::VBlank => (true, false),
            GPUMode::OAM => (false, true),
            GPUMode::VRAM => (true, true),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Palette {
    color: [Color; 4],
}

impl Default for Palette {
    fn default() -> Self {
        Palette {
            color: [COLOR_WHITE, COLOR_WHITE, COLOR_WHITE, COLOR_WHITE],
        }
    }
}

pub struct GPU {
    vram: [u8; VRAM_SIZE * 2],
    vram_bank: u8,
    oam: [u8; OAM_SIZE],
    framebuffer: [u32; WIDTH * HEIGHT],
    clock: usize,
    mode: GPUMode,
    palettes: [Palette; 8],
    pub(crate) line: u8,
    pub(crate) lcdc: BitField,
    pub(crate) stat: BitField,
    pub(crate) lyc: u8,
    pub(crate) scx: u8,
    pub(crate) scy: u8,
}

impl Default for GPU {
    fn default() -> Self {
        GPU::new()
    }
}

impl GPU {
    pub fn new() -> Self {
        GPU {
            vram: [0; VRAM_SIZE * 2],
            vram_bank: 0,
            oam: [0; OAM_SIZE],
            framebuffer: [COLOR_WHITE.into(); WIDTH * HEIGHT],
            clock: 0,
            mode: GPUMode::HBlank,
            palettes: [Palette::default(); 8],
            line: 0,
            lcdc: 0.into(),
            stat: 0.into(),
            lyc: 0,
            scx: 0,
            scy: 0,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            VRAM_START..=VRAM_END => {
                self.vram[((self.vram_bank as usize * VRAM_SIZE) + (addr - VRAM_START) as usize)]
            }
            OAM_START..=OAM_END => self.oam[(addr - OAM_START) as usize],
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            VRAM_START..=VRAM_END => {
                self.vram[((self.vram_bank as usize * VRAM_SIZE) + (addr - VRAM_START) as usize)] =
                    value
            }
            OAM_START..=OAM_END => self.oam[(addr - OAM_START) as usize] = value,
            _ => unreachable!(),
        }
    }

    pub fn set_bank(&mut self, bank: u8) {
        self.vram_bank = bank % 2;
    }

    // A color takes 2 bytes, where the low byte defines the red a part of the green,
    // and the high byte defines the rest of the green and the blue; each channel
    // takes 5 bits (0x00 to 0x1F).
    //   |-------------------------------| |-------------------------------|
    //   | Low byte                      | | High byte                     |
    //   |-------------------------------| |-------------------------------|
    //   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
    //   |-------------------------------| |-------------------------------|
    //   |        red        |        green        |        blue       | - |
    //   |-----------------------------------------------------------------|
    pub fn set_palette(&mut self, index: u8, value: u8) {
        let palette_idx = (index >> 3) & 7;
        let color_idx = (index >> 1) & 3;
        let high = index & 1 == 1;

        let color = self
            .palettes
            .get_mut(palette_idx as usize)
            .unwrap()
            .color
            .get_mut(color_idx as usize)
            .unwrap();

        if high {
            color.g |= (value & 3) << 3;
            color.b = (value >> 2) & 31;
        } else {
            color.r = value & 31;
            color.g |= (value >> 5) & 7
        }
    }

    pub fn get_palette(&self, index: u8) -> u8 {
        let palette_idx = (index >> 3) & 7;
        let color_idx = (index >> 1) & 3;
        let high = index & 1 == 1;

        let color = self
            .palettes
            .get(palette_idx as usize)
            .unwrap()
            .color
            .get(color_idx as usize)
            .unwrap();

        if high {
            let g = (color.g >> 3) & 3;
            let b = color.b << 2;

            g | b
        } else {
            let g = (color.g & 7) << 5;
            let r = color.r;

            g | r
        }
    }

    pub fn count_cycles(&mut self, cycles: usize) {
        self.clock += cycles * 4;
    }

    pub fn set_mode(&mut self, mode: GPUMode) {
        self.mode = mode;

        let (bit_0, bit_1) = mode.bits();
        self.stat.set(0, bit_0);
        self.stat.set(1, bit_1);
    }

    pub fn clear_ly(&mut self) {
        self.line = 0;
    }

    pub fn increment_ly(&mut self) {
        self.line += 1;
    }

    fn draw_background(&mut self) {}

    fn render_scanline(&mut self) {
        if self.lcdc.get(0) {
            // Background
        }
        if self.lcdc.get(5) {
            // window
        }
        if self.lcdc.get(1) {
            // sprites
        }
    }

    pub fn step(&mut self, gui: &mut GUI) -> Option<Interrupt> {
        let mut ret = None;

        match self.mode {
            GPUMode::HBlank => {
                if self.clock >= 204 {
                    self.clock = 0;
                    self.increment_ly();

                    if self.line == 143 {
                        self.set_mode(GPUMode::VBlank);
                        ret = Some(Interrupt::VBlank);
                        gui.update(&self.framebuffer[..]);
                    } else {
                        self.set_mode(GPUMode::OAM);
                    }
                }
            }
            GPUMode::VBlank => {
                if self.clock >= 456 {
                    self.clock = 0;
                    self.increment_ly();

                    if self.line >= 153 {
                        self.set_mode(GPUMode::OAM);
                        self.clear_ly();
                    }
                }
            }
            GPUMode::OAM => {
                if self.clock >= 80 {
                    self.clock = 0;
                    self.set_mode(GPUMode::VRAM);
                }
            }
            GPUMode::VRAM => {
                if self.clock >= 172 {
                    self.clock = 0;
                    self.set_mode(GPUMode::HBlank);

                    // render a scanline
                    self.render_scanline();
                    gui.update(&self.framebuffer[..]);
                }
            }
        }

        ret
    }
}
