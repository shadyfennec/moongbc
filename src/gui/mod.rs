use minifb::{Scale, Window, WindowOptions};

use crate::gpu::{HEIGHT, WIDTH};

pub struct GUI {
    window: Window,
}

impl Default for GUI {
    fn default() -> Self {
        GUI::new()
    }
}

impl GUI {
    pub fn new() -> Self {
        GUI {
            window: Window::new(
                "MoonGBC",
                WIDTH,
                HEIGHT,
                WindowOptions {
                    resize: true,
                    scale: Scale::X4,
                    ..WindowOptions::default()
                },
            )
            .expect("Failed to initialize window"),
        }
    }

    pub fn update(&mut self, buffer: &[u32]) {
        self.window
            .update_with_buffer(buffer, WIDTH, HEIGHT)
            .unwrap()
    }
}
