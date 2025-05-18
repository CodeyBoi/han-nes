use std::ops::Range;

pub type Addr = u16;
pub type Reg = u8;
pub type Opcode = u16;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Register(Reg),
    Immediate(u8),
}

pub enum DecodeError {
    InvalidSecondaryOpcode(u8, u8),
}

fn bitmask(bits: Range<u16>) -> u16 {
    let range = Range {
        start: bits.start,
        end: bits.end.min(16),
    };
    range.fold(0, |acc, i| acc | (0x1 << i))
}

fn get_bits(value: u16, bits: Range<u16>) -> u16 {
    let start = bits.start;
    (value & bitmask(bits)) >> start
}

/**
 * Vx means `registers[x]`
 */
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    /** Suspend execution */
    Suspend,

    /** Scroll display `n` lines down. */
    ScrollDown(u8),

    /** Clear the screen. */
    Clear,

    /** Return from a subroutine. */
    Return,

    /** Scroll display 4 pixels right. */
    ScrollRight,

    /** Scroll display 4 pixels left. */
    ScrollLeft,

    /** Exit CHIP Interpreter. */
    ExitChip,

    /** Disable extended screen mode. */
    DisableExtendedScreen,

    /** Enable extended screen mode for full screen graphics. */
    EnableExtendedScreen,

    /** Jump to location `addr`. */
    Jump(Addr),

    /** Call subroutine at `addr`. */
    Call(Addr),

    /** Skip next instruction if `Vx == v`. */
    SkipIfEqual { x: Reg, v: Value },

    /** Skip next instruction if `Vx != v`. */
    SkipIfNotEqual { x: Reg, v: Value },

    /** Set value of `Vx` to `v`. */
    Load { x: Reg, v: Value },

    /** Set value of `Vx` to `Vx + v`. Set `Vf = 1` if `v` is a register and `Vx + v > 255`. */
    Add { x: Reg, v: Value },

    /** Set value of `Vx` to `Vx | Vy`. */
    Or { x: Reg, y: Reg },

    /** Set value of `Vx` to `Vx & Vy`. */
    And { x: Reg, y: Reg },

    /** Set value of `Vx` to `Vx ^ Vy`. */
    Xor { x: Reg, y: Reg },

    /** Set `Vf = 1` if `Vx > Vy`. Then set value of `Vx` to `Vx - Vy`. */
    Sub { x: Reg, y: Reg },

    /** Set `Vf = 1` if least significant bit of `Vx` is one. Then set value of `Vx` to `Vx >> 1`. */
    ShiftRight { x: Reg, y: Reg },

    /** Set `Vf = 1` if `Vy > Vx`. Then set value of `Vx` to `Vx - Vy`. */
    SubNegative { x: Reg, y: Reg },

    /**  Set `Vf = 1` if most significant bit of `Vx` is one. Then set value of `Vx` to `Vx << 1`. */
    ShiftLeft { x: Reg, y: Reg },

    /** Set register `I` to `addr`. */
    LoadIndex(Addr),

    /** Jump to location `addr + V0`. */
    JumpOffset { x: Reg, addr: Addr },

    /** Set `Vx` to a newly generated random byte ANDed with `mask`. */
    Random { x: Reg, mask: u8 },

    /** Display an n-byte sprite starting at location `I` at `(Vx, Vy)`. Set `Vf = collision`. */
    Draw { x: Reg, y: Reg, n: u8 },

    /** Skip next instruction if key with the value in `Vx` is pressed. */
    SkipIfKeyPressed { x: Reg },

    /** Skip next instruction if key with the value in `Vx` is NOT pressed. */
    SkipIfKeyNotPressed { x: Reg },

    /** Set `Vx = DT`. */
    LoadDtIntoRegister { x: Reg },

    /** Wait for a key press, then store value of key in `Vx`. */
    LoadKeyPress { x: Reg },

    /** Set `DT = Vx`. */
    LoadRegisterIntoDt { x: Reg },

    /** Set `ST = Vx`. */
    LoadRegisterIntoSt { x: Reg },

    /** Set `I` to `I + Vx`. */
    AddIndex { x: Reg },

    /** Set `I` to location of digit sprite corresponding to value in `Vx`. */
    LoadDigitIndex { x: Reg },

    /** Set `I` to location of extended digit sprite corresponding to value in `Vx`. */
    LoadExtendedDigitIndex { x: Reg },

    /** Store BCD representation of `Vx` in location `I`, `I+1` and `I+2`. */
    LoadBcd { x: Reg },

    /** Store `V0` to `Vx` in memory, starting at location `I`. */
    StoreRegisters { x: Reg },

    /** Read registers `V0` to `Vx` from memory starting at location `I`. */
    LoadRegisters { x: Reg },

    /** Store registers `V0` to `Vx` in RPL user flags (x <= 7). */
    StoreRegistersRPL { x: Reg },

    /** Load registers `V0` to `Vx` from RPL user flags (x <= 7). */
    LoadRegistersRPL { x: Reg },
}

impl Instruction {
    pub fn decode(opcode: Opcode) -> Result<Instruction, DecodeError> {
        use Instruction as I;
        let code = get_bits(opcode, 12..16) as u8;
        let addr = get_bits(opcode, 0..12);
        let n = get_bits(opcode, 0..4) as u8;
        let x = get_bits(opcode, 8..12) as Reg;
        let y = get_bits(opcode, 4..8) as Reg;
        let byte = get_bits(opcode, 0..8) as u8;
        Ok(match code {
            0x0 => match y {
                0x0 => match n {
                    0x0 => I::Suspend,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
                },
                0xc => I::ScrollDown(n),
                0xe => match n {
                    0x0 => I::Clear,
                    0xe => I::Return,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
                },
                0xf => match n {
                    0xb => I::ScrollRight,
                    0xc => I::ScrollLeft,
                    0xd => I::ExitChip,
                    0xe => I::DisableExtendedScreen,
                    0xf => I::EnableExtendedScreen,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
                },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            0x1 => I::Jump(addr),
            0x2 => I::Call(addr),
            0x3 => I::SkipIfEqual {
                x,
                v: Value::Immediate(byte),
            },
            0x4 => I::SkipIfNotEqual {
                x,
                v: Value::Immediate(byte),
            },
            0x5 => match n {
                0x0 => I::SkipIfEqual {
                    x,
                    v: Value::Register(y),
                },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            0x6 => I::Load {
                x,
                v: Value::Immediate(byte),
            },
            0x7 => I::Add {
                x,
                v: Value::Immediate(byte),
            },
            0x8 => match n {
                0x0 => I::Load {
                    x,
                    v: Value::Register(y),
                },
                0x1 => I::Or { x, y },
                0x2 => I::And { x, y },
                0x3 => I::Xor { x, y },
                0x4 => I::Add {
                    x,
                    v: Value::Register(y),
                },
                0x5 => I::Sub { x, y },
                0x6 => I::ShiftRight { x, y },
                0x7 => I::SubNegative { x, y },
                0xe => I::ShiftLeft { x, y },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            0x9 => match n {
                0x0 => I::SkipIfNotEqual {
                    x,
                    v: Value::Register(y),
                },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            0xa => I::LoadIndex(addr),
            0xb => I::JumpOffset { x, addr },
            0xc => I::Random { x, mask: byte },
            0xd => I::Draw { x, y, n },
            0xe => match byte {
                0x9e => I::SkipIfKeyPressed { x },
                0xa1 => I::SkipIfKeyNotPressed { x },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            0xf => match byte {
                0x07 => I::LoadDtIntoRegister { x },
                0x0a => I::LoadKeyPress { x },
                0x15 => I::LoadRegisterIntoDt { x },
                0x18 => I::LoadRegisterIntoSt { x },
                0x1e => I::AddIndex { x },
                0x29 => I::LoadDigitIndex { x },
                0x30 => I::LoadExtendedDigitIndex { x },
                0x33 => I::LoadBcd { x },
                0x55 => I::StoreRegisters { x },
                0x65 => I::LoadRegisters { x },
                0x75 => I::StoreRegistersRPL { x },
                0x85 => I::LoadRegistersRPL { x },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, byte)),
            },
            _ => unreachable!("All 16-bit values are accounted for"),
        })
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
    fn test_get_bits() {
        let v = 0b1111000011110000;

        assert_eq!(get_bits(v, 0..5), 0b10000);
        assert_eq!(get_bits(v, 2..6), 0b1100);
        assert_eq!(get_bits(v, 0..8), 0b11110000);
        assert_eq!(get_bits(v, 8..16), 0b11110000);
    }
}
