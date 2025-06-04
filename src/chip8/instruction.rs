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

/// The opcodes are parsed as `0xXXXX`. Immediate values are denoted as `n`, `nn` and `nnn`,
/// where multiple `n`:s should be interpreted as an unsigned integer, big endian. References
/// to registers are denoted with `x` and `y`, and a register value is denoted as `Vx` or `Vy`.
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    /// `0x0000`: Suspend execution.
    Suspend,

    /// `0x00cn`: Scroll display `n` lines down.
    ScrollDown(u8),

    /// `0x00dn`: Scroll display `n` lines up.
    ScrollUp(u8),

    /// `0x00e0`: Clear the screen.
    Clear,

    /// `0x00ee`: Return from a subroutine.
    Return,

    /// `0x00fb`: Scroll display 4 pixels right.
    ScrollRight,

    /// `0x00fc`: Scroll display 4 pixels left.
    ScrollLeft,

    /// `0x00fd`: Exit CHIP Interpreter.
    ExitChip,

    /// `0x00fe`: Disable extended screen mode.
    DisableExtendedScreen,

    /// `0x00ff`: Enable extended screen mode for full screen graphics.
    EnableExtendedScreen,

    /// `0x1nnn`: Jump to location `nnn`.
    Jump(Addr),

    /// `0x2nnn`: Call subroutine at `nnn`.
    Call(Addr),

    /// `0x3xnn`/`0x5xy0`: Skip next instruction if `Vx == v`.
    SkipIfEqual { x: Reg, v: Value },

    /// `0x4xnn`/`0x9xy0`: Skip next instruction if `Vx != v`.
    SkipIfNotEqual { x: Reg, v: Value },

    /// `0x5xy2`: Save an inclusive range of registers `Vx`-`Vy` to memory starting at `I`.
    SaveRegisterRange { x: Reg, y: Reg },

    /// `0x5xy3`: Load an inclusive range of registers `Vx`-`Vy` to memory starting at `I`.
    LoadRegisterRange { x: Reg, y: Reg },

    /// `0x6xnn`/`0x8xy0`: Set value of `Vx` to `v`.
    Load { x: Reg, v: Value },

    /// `0x7xnn`/`0x8xy4`: Set value of `Vx` to `Vx + v`. Set `Vf = 1` if `v` is a register and `Vx + v > 255`.
    Add { x: Reg, v: Value },

    /// `0x8xy1`: Set value of `Vx` to `Vx | Vy`.
    Or { x: Reg, y: Reg },

    /// `0x8xy2`: Set value of `Vx` to `Vx & Vy`.
    And { x: Reg, y: Reg },

    /// `0x8xy3`: Set value of `Vx` to `Vx ^ Vy`.
    Xor { x: Reg, y: Reg },

    /// `0x8xy5`: Set `Vf = 1` if `Vx > Vy`. Then set value of `Vx` to `Vx - Vy`.
    Sub { x: Reg, y: Reg },

    /// `0x8xy6`: Set `Vf = 1` if least significant bit of `Vx` is one. Then set value of `Vx` to `Vx >> 1`.
    ShiftRight { x: Reg, y: Reg },

    /// `0x8xy7`: Set `Vf = 1` if `Vy > Vx`. Then set value of `Vx` to `Vx - Vy`.
    SubNegative { x: Reg, y: Reg },

    /// `0x8xye`: Set `Vf = 1` if most significant bit of `Vx` is one. Then set value of `Vx` to `Vx << 1`.
    ShiftLeft { x: Reg, y: Reg },

    /// `0xannn`: Set register `I` to `addr`.
    LoadIndex(Addr),

    /// `0xbnnn`: Jump to location `addr + V0`.
    JumpOffset { x: Reg, addr: Addr },

    /// `0xcxnn`: Set `Vx` to a newly generated random byte ANDed with `mask`.
    Random { x: Reg, mask: u8 },

    /// `0xdxyn`: Display an n-byte sprite starting at location `I` at `(Vx, Vy)`. Set `Vf = collision`.
    Draw { x: Reg, y: Reg, n: u8 },

    /// `0xex9e`: Skip next instruction if key with the value in `Vx` is pressed.
    SkipIfKeyPressed { x: Reg },

    /// `0xexa1`: Skip next instruction if key with the value in `Vx` is NOT pressed.
    SkipIfKeyNotPressed { x: Reg },

    /// `0xf000`: Load `addr` into `I`. If `addr` is `None` then advance `PC` 2 bytes and load 16-bit value at `PC` into `I`.
    LoadLongIndex(Option<Addr>),

    /// `0xf002`: Load 16 bytes starting at `I` into the audio pattern buffer.
    LoadAudio,

    /// `0xfx01`: Select 0 or more drawing planes via bitmask (0 <= `x` < 4).
    SelectPlane { mask: u8 },

    /// `0xfx07`: Set `Vx = DT`.
    LoadDtIntoRegister { x: Reg },

    /// `0xfx0a`: Wait for a key press, then store value of key in `Vx`.
    LoadKeyPress { x: Reg },

    /// `0xfx15`: Set `DT = Vx`.
    LoadRegisterIntoDt { x: Reg },

    /// `0xfx18`: Set `ST = Vx`.
    LoadRegisterIntoSt { x: Reg },

    /// `0xfx1e`: Set `I` to `I + Vx`.
    AddIndex { x: Reg },

    /// `0xfx29`: Set `I` to location of digit sprite corresponding to value in `Vx`.
    LoadFont { x: Reg },

    /// `0xfx30`: Set `I` to location of extended digit sprite corresponding to value in `Vx`.
    LoadHiResFont { x: Reg },

    /// `0xfx33`: Store BCD representation of `Vx` in location `I`, `I+1` and `I+2`.
    LoadBcd { x: Reg },

    /// `0xfx3a`: Set the audio pattern playback rate to `4000 * 2 ** ((Vx - 64) / 48)` Hz.
    SetPitch { x: Reg },

    /// `0xfx55`: Store `V0` to `Vx` in memory, starting at location `I`.
    StoreRegisters { x: Reg },

    /// `0xfx65`: Read registers `V0` to `Vx` from memory starting at location `I`.
    LoadRegisters { x: Reg },

    /// `0xfx75`: Store registers `V0` to `Vx` in RPL user flags.
    StoreRegistersRPL { x: Reg },

    /// `0xfx85`: Load registers `V0` to `Vx` from RPL user flags.
    LoadRegistersRPL { x: Reg },
}

impl Instruction {
    pub fn decode(opcode: Opcode) -> Result<Instruction, DecodeError> {
        use Instruction as I;
        let code = get_bits(opcode, 12..16) as u8;
        let nnn = get_bits(opcode, 0..12);
        let nn = get_bits(opcode, 0..8) as u8;
        let n = get_bits(opcode, 0..4) as u8;
        let x = get_bits(opcode, 8..12) as Reg;
        let y = get_bits(opcode, 4..8) as Reg;
        Ok(match code {
            0x0 => match y {
                0x0 => match n {
                    0x0 => I::Suspend,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
                },
                0xc => I::ScrollDown(n),
                0xd => I::ScrollUp(n),
                0xe => match n {
                    0x0 => I::Clear,
                    0xe => I::Return,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
                },
                0xf => match n {
                    0xb => I::ScrollRight,
                    0xc => I::ScrollLeft,
                    0xd => I::ExitChip,
                    0xe => I::DisableExtendedScreen,
                    0xf => I::EnableExtendedScreen,
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
                },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
            },
            0x1 => I::Jump(nnn),
            0x2 => I::Call(nnn),
            0x3 => I::SkipIfEqual {
                x,
                v: Value::Immediate(nn),
            },
            0x4 => I::SkipIfNotEqual {
                x,
                v: Value::Immediate(nn),
            },
            0x5 => match n {
                0x0 => I::SkipIfEqual {
                    x,
                    v: Value::Register(y),
                },
                0x2 => I::SaveRegisterRange { x, y },
                0x3 => I::LoadRegisterRange { x, y },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
            },
            0x6 => I::Load {
                x,
                v: Value::Immediate(nn),
            },
            0x7 => I::Add {
                x,
                v: Value::Immediate(nn),
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
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
            },
            0x9 => match n {
                0x0 => I::SkipIfNotEqual {
                    x,
                    v: Value::Register(y),
                },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
            },
            0xa => I::LoadIndex(nnn),
            0xb => I::JumpOffset { x, addr: nnn },
            0xc => I::Random { x, mask: nn },
            0xd => I::Draw { x, y, n },
            0xe => match nn {
                0x9e => I::SkipIfKeyPressed { x },
                0xa1 => I::SkipIfKeyNotPressed { x },
                _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
            },
            0xf => match nnn {
                0x000 => I::LoadLongIndex(None),
                0x002 => I::LoadAudio,
                _ => match nn {
                    0x01 => I::SelectPlane { mask: x },
                    0x07 => I::LoadDtIntoRegister { x },
                    0x0a => I::LoadKeyPress { x },
                    0x15 => I::LoadRegisterIntoDt { x },
                    0x18 => I::LoadRegisterIntoSt { x },
                    0x1e => I::AddIndex { x },
                    0x29 => I::LoadFont { x },
                    0x30 => I::LoadHiResFont { x },
                    0x33 => I::LoadBcd { x },
                    0x3a => I::SetPitch { x },
                    0x55 => I::StoreRegisters { x },
                    0x65 => I::LoadRegisters { x },
                    0x75 => I::StoreRegistersRPL { x },
                    0x85 => I::LoadRegistersRPL { x },
                    _ => return Err(DecodeError::InvalidSecondaryOpcode(code, nn)),
                },
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
