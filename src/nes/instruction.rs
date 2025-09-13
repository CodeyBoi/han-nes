use super::{Address, ShortAddress};

type Relative = i8;

/// Info about 6502 instructions have been taken from https://www.nesdev.org/wiki/Instruction_reference.
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    // Official instructions
    /// ADC: Adds the carry flag and a memory value to the accumulator.
    AddWithCarry(ByteLocation),

    /// AND: Performs bitwise AND on accumulator with a memory value.
    BitwiseAnd(ByteLocation),

    /// ASL: Shifts all bits in value left, moving the value of each bit into the next bit. Bit 7 is shifted into the carry flag, and bit 0 is cleared.
    ArithmeticShiftLeft(ByteLocation),

    /// BCC: Add value to program counter if carry flag is clear.
    BranchIfCarryClear(Relative),

    /// BCS: Add value to program counter if carry flag is set.
    BranchIfCarrySet(Relative),

    /// BEQ: Add value to program counter if zero flag is set.
    BranchIfEqual(Relative),

    /// BIT: Modfies flags, but does not change memory or registers. Zero flag is set if accumulator & memory value == 0. Bits 7 and 6 are loaded directly into the negative and overflow flags.
    BitTest(ByteLocation),

    /// BMI: Add value to program counter if negative flag is set.
    BranchIfMinus(Relative),

    /// BEQ: Add value to program counter if zero flag is clear.
    BranchIfNotEqual(Relative),

    /// BPL: Add value to program counter if negative flag is clear.
    BranchIfPlus(Relative),

    /// BRK: Software interrupt. Pushes program counter and flags to stack and sets program counter to $FFFE.
    Break,

    /// BVC: Add value to program counter if overflow flag is clear.
    BranchIfOverflowClear(Relative),

    /// BVC: Add value to program counter if overflow flag is set.
    BranchIfOverflowSet(Relative),

    /// CLC: Clear the carry flag.
    ClearCarry,

    /// CLD: Clear the decimal flag.
    ClearDecimal,

    /// CLI: Clear the interrupt disable flag.
    ClearInterruptDisable,

    /// CLV: Clear the overflow flag.
    ClearOverflow,

    /// CMP: Compares the accumulator to a memory value via a subtraction that sets status flags but doesn't modify any register values. NOTE: Does NOT affect the overflow flag.
    CompareAcc(ByteLocation),

    /// CPX: Like CompareAcc, but using the X register instead.
    CompareX(ByteLocation),

    /// CPY: Like CompareAcc, but using the Y register instead.
    CompareY(ByteLocation),

    /// DEC: Decrements a memory location. Cannot be used for decrementing the accumulator; ADC or SBC must be used instead. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementMemory(ByteLocation),

    /// DEX: Decrements the X register. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementX,

    /// DEY: Decrements the Y register. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementY,

    /// EOR: Exclusive bitwise OR on a memory value and the accumulator and writes to the accumulator.
    BitwiseExclusiveOr(ByteLocation),

    /// INC: Increments a memory location. Cannot be used for incrementing the accumulator; ADC or SBC must be used instead. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementMemory(ByteLocation),

    /// INX: Increments the X register. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementX,

    /// INY: Increments the Y register. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementY,

    /// JMP: Sets the program counter to a new value. If you want to return from that location, JSR (JumpToSubroutine) should be used instead.
    Jump(JumpAddress),

    /// JSR: Pushes the current program counter + 2 to the stack and sets it to a new value. Execution can then return by using RTS.
    JumpToSubroutine(Address),

    /// LDA: Loads a memory value into the accumulator.
    LoadAcc(ByteLocation),

    /// LDA: Loads a memory value into the X register.
    LoadX(ByteLocation),

    /// LDA: Loads a memory value into the Y register.
    LoadY(ByteLocation),

    /// LSR: Shifts all the bits in the accumulator or a memory value one position to the right. Lowest bit is shifted into the carry flag.
    LogicalShiftRight(ByteLocation),

    /// NOP: Does nothing. Used for padding.
    NoOperation,

    /// ORA: Bitwise ORs the accumulator and a memory value.
    BitwiseOr(ByteLocation),

    /// PHA: Push accumulator value to stack.
    PushAcc,

    /// PHP: Push status flag register to stack. Break flag is pushed as 1 (Software).
    PushProcessorStatus,

    /// PLA: Pops the top value from the stack and stores it in the accumulator.
    PullAcc,

    /// PLP: Pops the top value from the stack and stores it in the status flag register. The B and unused flags are ignored.
    PullProcessorStatus,

    /// ROL: Shifts a memory value or the accumulator to the left. The highest bit is rotated into the carry flag, and the carry flag is rotated into the lowest bit.
    RotateLeft(ByteLocation),

    /// ROL: Shifts a memory value or the accumulator to the right. The lowest bit is rotated into the carry flag, and the carry flag is rotated into the highest bit.
    RotateRight(ByteLocation),

    /// RTI: Returns from an interrupt handler by popping the status register and then the program counter from the stack.
    ReturnFromInterrupt,

    /// RTS: Returns from a subroutine by popping the program counter from stack and then incrementing the program counter.
    ReturnFromSubroutine,

    /// SBC: Subtracts a memory value and the bitwise negation of the carry flag from the accumulator.
    SubtractWithCarry(ByteLocation),

    /// SEC: Sets the carry flag.
    SetCarry,

    /// SED: Sets the decimal mode flag.
    SetDecimal,

    /// SEI: Sets the interrupt disable flag.
    SetInterruptDisable,

    /// STA: Stores the accumulator value into memory.
    StoreAcc(ByteLocation),

    /// STX: Stores the X register value into memory.
    StoreX(ByteLocation),

    /// STY: Stores the Y register value into memory.
    StoreY(ByteLocation),

    /// TAX: Copies the accumulator value into the X register.
    TransferAccToX,

    /// TAY: Copies the accumulator value into the Y register.
    TransferAccToY,

    /// TSX: Copies the stack pointer value into the X register.
    TransferStackPointerToX,

    /// TXA: Copies the X register value into the accumulator.
    TransferXToAcc,

    /// TXS: Copies the X register value into the stack pointer.
    TransferXToStackPointer,

    /// TYA: Copies the Y register value into the accumulator.
    TransferYToAcc,
}

/// 6502 Addressing Modes. Defines different possible formats for fetching instructions arguments. More info about this can be found at https://www.nesdev.org/obelisk-6502-guide/addressing.html.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteLocation {
    /// The value in the A register.
    Accumulator,

    /// Allows the programmer to directly specify an 8-bit value. Indicated by a `#` symbol followed by the value (e.g. #F8).
    Immediate(u8),

    /// Memory access limited to the first 256 bytes of memory (i.e. $0000 to $00FF).
    ZeroPage(ShortAddress),

    /// Same as ZeroPage, but the X register is added to the address beforehand. If the addition is greater than #FF, it wraps around.
    ZeroPageX(ShortAddress),

    /// Same as ZeroPageX, but using the Y register instead. Can only be used with the LDX and STX instructions.
    ZeroPageY(ShortAddress),

    /// Memory access using a full 16-bit address.
    Absolute(Address),

    /// Same as Absolute, but the X register is added to the address beforehand.
    AbsoluteX(Address),

    /// Same as Absolute, but the Y register is added to the address beforehand.
    AbsoluteY(Address),

    /// Reads the byte from the address held in two bytes in zero page at address + X (little-endian). Wraps around if addition is greater than $FF.
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpAddress {
    Absolute(Address),
    Indirect(Address),
}

impl ByteLocationType {
    fn from_opcode(opcode: u8) -> Self {
        use ByteLocationType as BT;
        // Opcodes grouped by addressing mode was found at https://www.pagetable.com/c64ref/6502/?tab=3#
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

pub trait Take: Sized {
    fn take_one(self) -> Result<(Self, u8), DecodeError>;
    fn take<const N: usize>(self) -> Result<(Self, [u8; N]), DecodeError>;
    fn take_n(self, n: usize) -> Result<(Self, Self), DecodeError>;
}

impl<'a> Take for &'a [u8] {
    fn take_one(self) -> Result<(Self, u8), DecodeError> {
        let (first, rest) = self.split_first().ok_or(DecodeError::NeedsMoreData(1))?;
        Ok((rest, *first))
    }

    fn take<const N: usize>(self) -> Result<(&'a [u8], [u8; N]), DecodeError> {
        let (taken_bytes, data) = self
            .split_at_checked(N)
            .ok_or(DecodeError::NeedsMoreData(N - self.len()))?;
        Ok((
            data,
            taken_bytes
                .try_into()
                .expect("if slice was too short it would have been caught above"),
        ))
    }

    fn take_n(self, n: usize) -> Result<(Self, Self), DecodeError> {
        let (taken_bytes, data) = self
            .split_at_checked(n)
            .ok_or(DecodeError::NeedsMoreData(n - self.len()))?;
        Ok((data, taken_bytes))
    }
}

macro_rules! build_instruction_with_branch {
    ($instruction:ident, $data:ident) => {{
        let (data, offset) = $data.take_one()?;
        (data, Instruction::$instruction(offset as i8))
    }};
}

macro_rules! build_instruction_with_location {
    ($instruction:ident, $opcode:ident, $data:ident) => {{
        let (data, location) = Self::location($data, ByteLocationType::from_opcode($opcode))?;
        (data, Instruction::$instruction(location))
    }};
}

#[derive(PartialEq, Eq, Debug)]
pub enum DecodeError {
    InvalidOpcode(u8),
    NeedsMoreData(usize),
}

impl Instruction {
    pub fn decode(data: &[u8]) -> Result<(&[u8], Instruction), DecodeError> {
        let (data, opcode) = data.take_one()?;
        let (data, instruction) = match opcode {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                build_instruction_with_location!(AddWithCarry, opcode, data)
            }
            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                build_instruction_with_location!(BitwiseAnd, opcode, data)
            }
            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
                build_instruction_with_location!(ArithmeticShiftLeft, opcode, data)
            }
            0x90 => build_instruction_with_branch!(BranchIfCarryClear, data),
            0xB0 => build_instruction_with_branch!(BranchIfCarrySet, data),
            0xF0 => build_instruction_with_branch!(BranchIfEqual, data),
            0x24 | 0x2C => build_instruction_with_location!(BitTest, opcode, data),
            0x30 => build_instruction_with_branch!(BranchIfMinus, data),
            0xD0 => build_instruction_with_branch!(BranchIfNotEqual, data),
            0x10 => build_instruction_with_branch!(BranchIfPlus, data),
            0x00 => {
                // The return address that is pushed to the stack skips the following byte (current program counter + 2), so we shift data by 1.
                let (data, _) = data.take_one()?;
                (data, Instruction::Break)
            }
            0x50 => build_instruction_with_branch!(BranchIfOverflowClear, data),
            0x70 => build_instruction_with_branch!(BranchIfOverflowSet, data),
            0x18 => (data, Instruction::ClearCarry),
            0xD8 => (data, Instruction::ClearDecimal),

            _ => return Err(DecodeError::InvalidOpcode(opcode)),
        };

        Ok((data, instruction))
    }

    fn location(
        data: &[u8],
        byte_type: ByteLocationType,
    ) -> Result<(&[u8], ByteLocation), DecodeError> {
        use ByteLocation as B;
        use ByteLocationType as BT;
        match byte_type {
            BT::Accumulator => Ok((data, B::Accumulator)),
            BT::Immediate => {
                let (data, value) = data.take_one()?;
                Ok((data, B::Immediate(value)))
            }
            BT::ZeroPage => {
                let (data, addr) = data.take_one()?;
                Ok((data, B::ZeroPage(addr)))
            }
            BT::ZeroPageX => {
                let (data, addr) = data.take_one()?;
                Ok((data, B::ZeroPageX(addr)))
            }
            BT::ZeroPageY => {
                let (data, addr) = data.take_one()?;
                Ok((data, B::ZeroPageY(addr)))
            }
            BT::Absolute => {
                let (data, [low, high]) = data.take()?;
                Ok((data, B::Absolute(((high as u16) << 8) | low as u16)))
            }
            BT::AbsoluteX => {
                let (data, [low, high]) = data.take()?;
                Ok((data, B::AbsoluteX(((high as u16) << 8) | low as u16)))
            }
            BT::AbsoluteY => {
                let (data, [low, high]) = data.take()?;
                Ok((data, B::AbsoluteY(((high as u16) << 8) | low as u16)))
            }
            BT::IndirectX => {
                let (data, addr) = data.take_one()?;
                Ok((data, B::IndirectX(addr)))
            }
            BT::IndirectY => {
                let (data, addr) = data.take_one()?;
                Ok((data, B::IndirectY(addr)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ByteLocation as B;
    use Instruction as I;

    #[test]
    fn test_decode() {
        let data = [0x16, 0x55, 0x30, -0x12i8 as u8, 0x3D, 0xAD, 0xDE].as_slice();
        assert_eq!(
            I::decode(data),
            Ok((&data[2..], I::ArithmeticShiftLeft(B::ZeroPageX(0x55))))
        );

        assert_eq!(
            I::decode(&data[2..]),
            Ok((&data[4..], I::BranchIfMinus(-0x12)))
        );

        assert_eq!(
            I::decode(&data[4..]),
            Ok((&data[7..], I::BitwiseAnd(B::AbsoluteX(0xDEAD))))
        );

        assert_eq!(I::decode(&data[7..]), Err(DecodeError::NeedsMoreData(1)));
    }
}
