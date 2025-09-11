use super::{Address, ShortAddress};

/// Info about 6502 instructions have been taken from https://www.nesdev.org/wiki/Instruction_reference.
#[derive(Debug)]
pub enum Instruction {
    // Official instructions
    /// ADC: Adds the carry flag and a memory value to the accumulator.
    AddWithCarry(ByteLocation),

    /// AND: Performs bitwise AND on accumulator with a memory value.
    BitwiseAnd(ByteLocation),

    /// ASL: Shifts all bits in value left, moving the value of each bit into the next bit. Bit 7 is shifted into the carry flag, and bit 0 is cleared.
    ArithmeticShiftLeft(ByteLocation),

    /// BCC: Add value to program counter if carry flag is clear.
    BranchIfCarryClear(i8),

    /// BCS: Add value to program counter if carry flag is set.
    BranchIfCarrySet(i8),

    /// BEQ: Add value to program counter if zero flag is set.
    BranchIfEqual(i8),

    /// BIT: Modfies flags, but does not change memory or registers. Zero flag is set if accumulator & memory value == 0. Bits 7 and 6 are loaded directly into the negative and overflow flags.
    BitTest(ByteLocation),

    /// BMI: Add value to program counter if negative flag is set.
    BranchIfMinus(i8),

    /// BEQ: Add value to program counter if zero flag is clear.
    BranchIfNotEqual(i8),

    /// BPL: Add value to program counter if negative flag is clear.
    BranchIfPlus(i8),

    /// BRK: Software interrupt. Pushes program counter and flags to stack and sets program counter to $FFFE.
    Break,

    /// BVC: Add value to program counter if overflow flag is clear.
    BranchIfOverflowClear(i8),

    /// BVC: Add value to program counter if overflow flag is set.
    BranchIfOverflowSet(i8),

    /// CLC: Clear the carry flag.
    ClearCarry,

    /// CLD: Clear the decimal flag.
    ClearDecimal,
}

/// 6502 Addressing Modes. Defines different possible formats for fetching instructions arguments. More info about this can be found at https://www.nesdev.org/obelisk-6502-guide/addressing.html.
///
/// The code $XXX before each addressing mode shows of which form the opcode is (i.e. $CE means the opcode ends with C or E).
#[derive(Debug, Clone, Copy)]
pub enum ByteLocation {
    /// $A: The value in the A register.
    Accumulator,

    /// $029: Allows the programmer to directly specify an 8-bit value. Indicated by a `#` symbol followed by the value (e.g. #F8).
    Immediate(u8),

    /// $memory access limited to the first 256 bytes of memory (i.e. $0000 to $00FF).
    ZeroPage(ShortAddress),

    /// Same as ZeroPage, but the X register is added to the address beforehand. If the addition is greater than #FF, it wraps around.
    ZeroPageX(ShortAddress),

    /// Same as ZeroPageX, but using the Y register instead. Can only be used with the LDX and STX instructions.
    ZeroPageY(ShortAddress),

    /// $CDEA memory access using a full 16-bit address.
    Absolute(Address),

    /// Same as Absolute, but the X register is added to the address beforehand.
    AbsoluteX(Address),

    /// Same as Absolute, but the Y register is added to the address beforehand.
    AbsoluteY(Address),

    /// Reads the byte from the address held in zero page at address + X (little-endian). Wraps around if addition is greater than $FF.
    IndirectX(ShortAddress),

    /// Same as IndirectX, but using the Y register instead.
    IndirectY(ShortAddress),
}

enum ByteLocationType {
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
}

impl ByteLocationType {
    fn from_opcode(opcode: u8) -> Self {
        use ByteLocationType as BT;
        match opcode {
            0x0a | 0x4a | 0x2a | 0x6a => BT::Accumulator,
            0x69 | 0x29 | 0xc9 | 0xe0 | 0xc0 | 0x49 | 0xa9 | 0xa2 | 0xa0 | 0x09 | 0xe9 => {
                BT::Immediate
            }
            0x6D | 0x2D | 0x0E | 0x2C | 0xCD | 0xEC | 0xCC | 0xCE | 0x4D | 0xEE | 0x4C | 0x20
            | 0xAD | 0xAE | 0xAC | 0x4E | 0x0D | 0x2E | 0x6E | 0xED | 0x8D | 0x8E | 0x8C => {
                BT::Absolute
            }
            0x7D | 0x3D | 0x1E | 0xDD | 0xDE | 0x5D | 0xFE | 0xBD | 0xBC | 0x5E | 0x1D | 0x3E
            | 0x7E | 0xFD | 0x9D => BT::AbsoluteX,
            0x79 | 0x39 | 0xD9 | 0x59 | 0xB9 | 0xBE | 0x19 | 0xF9 | 0x99 => BT::AbsoluteY,
            0x65 | 0x25 | 0x06 | 0x24 | 0xC5 | 0xE4 | 0xC4 | 0xC6 | 0x45 | 0xE6 | 0xA5 | 0xA6
            | 0xA4 | 0x46 | 0x05 | 0x26 | 0x66 | 0xE5 | 0x85 | 0x86 | 0x84 => BT::ZeroPage,
            0x75 | 0x35 | 0x16 | 0xD5 | 0xD6 | 0x55 | 0xF6 | 0xB5 | 0xB4 | 0x56 | 0x15 | 0x36
            | 0x76 | 0xF5 | 0x95 | 0x94 => BT::ZeroPageX,
            0xB6 | 0x96 => BT::ZeroPageY,
            0x61 | 0x21 | 0xC1 | 0x41 | 0xA1 | 0x01 | 0xE1 | 0x81 => BT::IndirectX,
            0x71 | 0x31 | 0xD1 | 0x51 | 0xB1 | 0x11 | 0xF1 | 0x91 => BT::IndirectY,
            _ => panic!(
                "tried to get byte location type of invalid opcode ${:X}",
                opcode
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum WordLocation {
    /// Reads the two bytes at address (little-endian).
    Indirect(Address),
}

trait Take: Sized {
    fn take_one(self) -> (Self, u8);
    fn take<const N: usize>(self) -> (Self, [u8; N]);
}

impl Take for &[u8] {
    fn take_one(self) -> (Self, u8) {
        let (first, rest) = self.split_first().expect("reached end of RAM");
        (rest, *first)
    }

    fn take<const N: usize>(self) -> (Self, [u8; N]) {
        let (taken_bytes, data) = self.split_at_checked(N).expect("reached end of RAM");
        (
            data,
            taken_bytes
                .try_into()
                .expect("if slice was too short it would have been caught above"),
        )
    }
}

impl Instruction {
    pub fn decode(data: &[u8]) -> Option<(&[u8], Instruction)> {
        use ByteLocationType as BT;
        use Instruction as I;
        let (data, opcode) = data.take_one();
        let (data, instruction) = match opcode {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                let (data, location) = Self::location(data, BT::from_opcode(opcode));
                (data, I::AddWithCarry(location))
            }
            _ => panic!("opcode ${:X} not implemented", opcode),
        };

        Some((data, instruction))
    }

    fn location(data: &[u8], byte_type: ByteLocationType) -> (&[u8], ByteLocation) {
        use ByteLocation as B;
        use ByteLocationType as BT;
        match byte_type {
            BT::Accumulator => (data, B::Accumulator),
            BT::Immediate => {
                let (data, value) = data.take_one();
                (data, B::Immediate(value))
            }
            BT::ZeroPage => {
                let (data, addr) = data.take_one();
                (data, B::ZeroPage(addr))
            }
            BT::ZeroPageX => {
                let (data, addr) = data.take_one();
                (data, B::ZeroPageX(addr))
            }
            BT::ZeroPageY => {
                let (data, addr) = data.take_one();
                (data, B::ZeroPageY(addr))
            }
            BT::Absolute => {
                let (data, [low, high]) = data.take();
                (data, B::Absolute(((high as u16) << 8) | low as u16))
            }
            BT::AbsoluteX => {
                let (data, [low, high]) = data.take();
                (data, B::AbsoluteX(((high as u16) << 8) | low as u16))
            }
            BT::AbsoluteY => {
                let (data, [low, high]) = data.take();
                (data, B::AbsoluteY(((high as u16) << 8) | low as u16))
            }
            BT::IndirectX => {
                let (data, addr) = data.take_one();
                (data, B::IndirectX(addr))
            }
            BT::IndirectY => {
                let (data, addr) = data.take_one();
                (data, B::IndirectY(addr))
            }
        }
    }
}
