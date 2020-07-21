use crate::cpu::CPU;

/// Represents a byte, where each bit is accessible as a boolean value.
#[derive(Copy, Clone, Default)]
pub struct BitField {
    value: u8,
}

impl From<u8> for BitField {
    fn from(value: u8) -> Self {
        BitField { value }
    }
}

impl From<BitField> for u8 {
    fn from(b: BitField) -> Self {
        b.value
    }
}

impl BitField {
    /// Returns the boolean value of a bit in the BitField.
    pub fn get(&self, bit: u8) -> bool {
        self.value & (1 << bit) == (1 << bit)
    }

    /// Sets a bit in the BitField to a boolean value.
    pub fn set(&mut self, bit: u8, value: bool) {
        if value {
            self.value |= 1 << bit;
        } else {
            self.value &= !(1 << bit);
        }
    }

    /// Toggles a bit in the BitField.
    pub fn toggle(&mut self, bit: u8) {
        self.value ^= 1 << bit;
    }

    /// Writes an entire 8-bit value, overwriting the underlying value.
    pub fn write(&mut self, value: u8) {
        self.value = value
    }
}

/// Represents something that holds data that can be read.
pub trait Src<T> {
    /// Tries to read a value, returning it if it's possible, or `None` otherwise.
    fn try_read(&self, cpu: &CPU) -> Option<T>;

    /// Reads a value from the source.
    /// ### Panics
    /// The function panics if the value cannot be read.
    fn read(&self, cpu: &CPU) -> T {
        self.try_read(cpu).unwrap()
    }
}

/// Represents something that holds data that can be written to.
pub trait Dst<T> {
    fn try_write(&self, cpu: &mut CPU, value: T) -> Result<(), String>;
    /// Writes a value to the destination.
    fn write(&self, cpu: &mut CPU, value: T) {
        self.try_write(cpu, value).unwrap()
    }

    /// Adds something to the destination.
    fn add(&self, cpu: &mut CPU, inc: T)
    where
        Self: Src<T>,
        T: std::ops::Add<Output = T>,
    {
        self.write(cpu, self.read(cpu) + inc)
    }

    /// Subtracts something from the destination.
    fn sub(&self, cpu: &mut CPU, dec: T)
    where
        Self: Src<T>,
        T: std::ops::Sub<Output = T>,
    {
        self.write(cpu, self.read(cpu) - dec)
    }
}

impl Src<u8> for u8 {
    fn try_read(&self, _: &CPU) -> Option<u8> {
        Some(*self)
    }
}

impl Src<u16> for u16 {
    fn try_read(&self, _: &CPU) -> Option<u16> {
        Some(*self)
    }
}

/// Represents a value that can be either one type, or another.
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

impl<A, B> Either<A, B> {
    /// Returns an option containing `Some(A)` if the value was a left type,
    /// and `None` otherwise.
    pub fn left(self) -> Option<A> {
        match self {
            Either::Left(a) => Some(a),
            Either::Right(_) => None,
        }
    }

    /// Returns an option containing `Some(B)` if the value was a right type,
    /// and `None` otherwise.
    pub fn right(self) -> Option<B> {
        match self {
            Either::Left(_) => None,
            Either::Right(b) => Some(b),
        }
    }
}
