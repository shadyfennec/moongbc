/// Helper trait used to transform a hexadecimal value encded as a `String`
/// into its decoded value. This trait is implemented for the `u8` and `u16`
/// primitive types, as they are the only two types used in the GBC.
pub trait FromHexString: Sized {
    /// Tries to convert a `String` into `Self`, assuming Self has
    /// a hexadecimal representation.
    ///
    /// This function handles the following patterns:
    /// - A plain, decimal number: "108" -> 108
    /// - A plain, hexadecimal number: "AE" -> 174
    /// - A hexadecimal value prefixed by '0x': "0xBC04" -> 48132
    fn from_hex_string(input: String) -> Result<Self, ()>;
}

impl FromHexString for u8 {
    fn from_hex_string(input: String) -> Result<Self, ()> {
        input.parse::<u8>().or_else(|_| {
            u8::from_str_radix(input.as_str(), 16).or_else(|_| {
                if input.starts_with("0x") {
                    u8::from_str_radix(input.chars().skip(2).collect::<String>().as_str(), 16)
                        .map_err(|_| ())
                } else {
                    Err(())
                }
            })
        })
    }
}

impl FromHexString for u16 {
    fn from_hex_string(input: String) -> Result<Self, ()> {
        input.parse::<u16>().or_else(|_| {
            u16::from_str_radix(input.as_str(), 16).or_else(|_| {
                if input.starts_with("0x") {
                    u16::from_str_radix(input.chars().skip(2).collect::<String>().as_str(), 16)
                        .map_err(|_| ())
                } else {
                    Err(())
                }
            })
        })
    }
}
