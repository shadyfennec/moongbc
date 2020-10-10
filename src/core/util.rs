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
