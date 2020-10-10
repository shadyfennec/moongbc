use std::fmt;

use crate::{
    memory_map::{Interconnect, Mem},
    register::Flag,
    register::Reg16,
    register::Reg8,
    register::Registers,
};

#[derive(Debug, Copy, Clone)]
pub struct Watcher<T, S> {
    pub(crate) src: S,
    pub(crate) original: T,
}

impl Watcher<u8, Mem> {
    pub fn new(src: Mem, memory: &Interconnect, registers: &Registers) -> Self {
        Watcher {
            src,
            original: src.read(memory, registers),
        }
    }

    pub fn check(&self, memory: &Interconnect, registers: &Registers) -> bool {
        self.src.read(memory, registers) != self.original
    }

    pub fn refresh(&mut self, memory: &Interconnect, registers: &Registers) {
        self.original = self.src.read(memory, registers);
    }
}

impl Watcher<bool, Flag> {
    pub fn new(src: Flag, registers: &Registers) -> Self {
        Watcher {
            src: src.clone(),
            original: src.read(registers),
        }
    }

    pub fn check(&self, registers: &Registers) -> bool {
        self.src.read(registers) != self.original
    }

    pub fn refresh(&mut self, registers: &Registers) {
        self.original = self.src.read(registers);
    }
}

impl Watcher<u8, Reg8> {
    pub fn new(src: Reg8, registers: &Registers) -> Self {
        Watcher {
            src,
            original: src.read(registers),
        }
    }

    pub fn check(&self, registers: &Registers) -> bool {
        self.src.read(registers) != self.original
    }

    pub fn refresh(&mut self, registers: &Registers) {
        self.original = self.src.read(registers);
    }
}

impl Watcher<u16, Reg16> {
    pub fn new(src: Reg16, registers: &Registers) -> Self {
        Watcher {
            src,
            original: src.read(registers),
        }
    }

    pub fn check(&self, registers: &Registers) -> bool {
        self.src.read(registers) != self.original
    }

    pub fn refresh(&mut self, registers: &Registers) {
        self.original = self.src.read(registers);
    }
}

#[derive(Clone)]
pub enum WatchKind {
    Reg8(Watcher<u8, Reg8>),
    Reg16(Watcher<u16, Reg16>),
    Mem(Watcher<u8, Mem>),
    Flag(Watcher<bool, Flag>),
}

impl WatchKind {
    pub fn check(&self, memory: &Interconnect, registers: &Registers) -> bool {
        match self {
            WatchKind::Reg8(w) => w.check(registers),
            WatchKind::Reg16(w) => w.check(registers),
            WatchKind::Mem(w) => w.check(memory, registers),
            WatchKind::Flag(w) => w.check(registers),
        }
    }

    pub fn refresh(&mut self, memory: &Interconnect, registers: &Registers) {
        match self {
            WatchKind::Reg8(w) => w.refresh(registers),
            WatchKind::Reg16(w) => w.refresh(registers),
            WatchKind::Mem(w) => w.refresh(memory, registers),
            WatchKind::Flag(w) => w.refresh(registers),
        }
    }
}

impl fmt::Display for WatchKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WatchKind::Reg8(w) => write!(f, "Watching register {}", w.src),
            WatchKind::Reg16(w) => write!(f, "Watching register {}", w.src),
            WatchKind::Mem(w) => write!(f, "Watching address {}", w.src),
            WatchKind::Flag(w) => write!(f, "Watching flag {}", w.src),
        }
    }
}

#[derive(Clone)]
pub enum Breakpoint {
    Address(u16),
    Opcode(u8),
    Watch(WatchKind),
}

impl Breakpoint {
    pub fn check(&self, memory: &Interconnect, registers: &Registers) -> bool {
        match self {
            Breakpoint::Address(addr) => Reg16::PC.read(registers) == *addr,
            Breakpoint::Opcode(op) => memory.read(Reg16::PC.read(registers)) == *op,
            Breakpoint::Watch(w) => w.check(memory, registers),
        }
    }

    pub fn to_string(&self, memory: &Interconnect, registers: &Registers) -> String {
        let reached = if self.check(memory, registers) {
            "(Reached)"
        } else {
            ""
        };

        format!("{} {}", self, reached)
    }
}

impl fmt::Display for Breakpoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Breakpoint::Address(addr) => write!(f, "Break at address 0x{:04x}", addr),
            Breakpoint::Opcode(op) => write!(f, "Break at opcode 0x{:02x}", op),
            Breakpoint::Watch(w) => write!(f, "{}", w.to_string()),
        }
    }
}
