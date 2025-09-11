use std::ops::{BitAnd, BitOr, Range, Shl, Shr};

pub fn bitmask(bits: Range<u16>) -> u16 {
    let range = Range {
        start: bits.start,
        end: bits.end.min(16),
    };
    range.fold(0, |acc, i| acc | (0x1 << i))
}

pub trait BitAddressable:
    Copy + Sized + BitOr<Output = Self> + Shl<Output = Self> + Shr<u8, Output = Self> + From<u8>
{
    fn bit(self, bit: u8) -> bool;
    fn bits(self, bits: Range<u8>) -> Self {
        let start = bits.start;
        bits.fold(Into::<Self>::into(0u8), |acc, i| {
            acc | (if self.bit(i) {
                Into::<Self>::into(1u8)
            } else {
                Into::<Self>::into(0u8)
            } << i.into())
        }) >> start
    }
}

impl<T> BitAddressable for T
where
    T: Copy
        + From<u8>
        + Shr<u8, Output = Self>
        + BitOr<Output = Self>
        + BitAnd<Output = Self>
        + Shl<Output = Self>
        + PartialEq,
{
    fn bit(self, bit: u8) -> bool {
        ((self >> bit) & 1u8.into()) != 0u8.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitmask() {
        assert_eq!(bitmask(0..5), 0b00011111);
        assert_eq!(bitmask(1..6), 0b00111110);
        assert_eq!(bitmask(3..13), 0b1111111111000);
        assert_eq!(bitmask(0..16), 0b1111111111111111);
    }

    #[test]
    fn test_bit_addressable() {
        let v = 0b01101100;

        assert!(!v.bit(0));
        assert!(!v.bit(1));
        assert!(v.bit(2));
        assert!(v.bit(3));
        assert!(!v.bit(4));
        assert!(v.bit(5));
        assert!(v.bit(6));
        assert!(!v.bit(7));
    }

    #[test]
    fn test_get_bits() {
        let v = 0b1111000011110000u16;

        assert_eq!(v.bits(0..5), 0b10000);
        assert_eq!(v.bits(2..6), 0b1100);
        assert_eq!(v.bits(0..8), 0b11110000);
        assert_eq!(v.bits(8..16), 0b11110000);
    }
}
