use std::ops::Range;

pub fn bitmask(bits: Range<u16>) -> u16 {
    let range = Range {
        start: bits.start,
        end: bits.end.min(16),
    };
    range.fold(0, |acc, i| acc | (0x1 << i))
}

pub fn get_bits(value: u16, bits: Range<u16>) -> u16 {
    let start = bits.start;
    (value & bitmask(bits)) >> start
}

pub fn get_bit(value: u8, bit: u8) -> bool {
    value & (0x1 << bit) != 0
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
    fn test_get_bits() {
        let v = 0b1111000011110000;

        assert_eq!(get_bits(v, 0..5), 0b10000);
        assert_eq!(get_bits(v, 2..6), 0b1100);
        assert_eq!(get_bits(v, 0..8), 0b11110000);
        assert_eq!(get_bits(v, 8..16), 0b11110000);
    }
}
