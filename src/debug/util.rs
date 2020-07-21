pub trait FromHexString: Sized {
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
