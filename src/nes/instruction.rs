use thiserror::Error;

use crate::nes::Address;
use crate::nes::cpu::ShortAddress;

type Offset = i8;

/// Info about 6502 instructions have been taken from https://www.nesdev.org/wiki/Instruction_reference.
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    /// Instruction with pre-defined arguments, e.g. Break or IncrementX.
    Implied(ImpliedInstruction),

    /// Instruction which targets and reads from or writes to a register or location in memory.
    Memory {
        instruction: MemoryInstruction,
        target: MemoryTarget,
    },

    /// Instruction which checks for a condition and then applies an offset to the program counter depending on the result.
    Branch {
        instruction: BranchInstruction,
        offset: Offset,
    },

    /// JMP: Sets the program counter to a new value. If you want to return from that location, JSR (JumpToSubroutine) should be used instead.
    Jump(JumpAddress),

    /// JSR: Pushes the current program counter + 2 to the stack and sets it to a new value. Execution can then return by using RTS.
    JumpToSubroutine(Address),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImpliedInstruction {
    // Official instructions
    /// BRK: Software interrupt. Pushes program counter and flags to stack and sets program counter to $FFFE.
    Break,

    /// CLC: Clear the carry flag.
    ClearCarry,

    /// CLD: Clear the decimal flag.
    ClearDecimal,

    /// CLI: Clear the interrupt disable flag.
    ClearInterruptDisable,

    /// CLV: Clear the overflow flag.
    ClearOverflow,

    /// DEX: Decrements the X register. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementX,

    /// DEY: Decrements the Y register. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementY,

    /// INX: Increments the X register. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementX,

    /// INY: Increments the Y register. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementY,

    /// NOP: Does nothing. Used for padding.
    NoOperation,

    /// PHA: Push accumulator value to stack.
    PushAcc,

    /// PHP: Push status flag register to stack. Break flag is pushed as 1 (Software).
    PushProcessorStatus,

    /// PLA: Pops the top value from the stack and stores it in the accumulator.
    PullAcc,

    /// PLP: Pops the top value from the stack and stores it in the status flag register. The B and unused flags are ignored.
    PullProcessorStatus,

    /// RTI: Returns from an interrupt handler by popping the status register and then the program counter from the stack.
    ReturnFromInterrupt,

    /// RTS: Returns from a subroutine by popping the program counter from stack and then incrementing the program counter.
    ReturnFromSubroutine,

    /// SEC: Sets the carry flag.
    SetCarry,

    /// SED: Sets the decimal mode flag.
    SetDecimal,

    /// SEI: Sets the interrupt disable flag.
    SetInterruptDisable,

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

#[derive(Debug, PartialEq, Eq)]
pub enum MemoryInstruction {
    /// ADC: Adds the carry flag and a memory value to the accumulator.
    AddWithCarry,

    /// AND: Performs bitwise AND on accumulator with a memory value.
    BitwiseAnd,

    /// ASL: Shifts all bits in value left, moving the value of each bit into the next bit. Bit 7 is shifted into the carry flag, and bit 0 is cleared.
    ArithmeticShiftLeft,

    /// BIT: Modfies flags, but does not change memory or registers. Zero flag is set if accumulator & memory value == 0. Bits 7 and 6 are loaded directly into the negative and overflow flags.
    BitTest,

    /// CMP: Compares the accumulator to a memory value via a subtraction that sets status flags but doesn't modify any register values. NOTE: Does NOT affect the overflow flag.
    CompareAcc,

    /// CPX: Like CompareAcc, but using the X register instead.
    CompareX,

    /// CPY: Like CompareAcc, but using the Y register instead.
    CompareY,

    /// DEC: Decrements a memory location. Cannot be used for decrementing the accumulator; ADC or SBC must be used instead. NOTE: Does NOT affect the carry nor overflow flags.
    DecrementMemory,

    /// EOR: Exclusive bitwise OR on a memory value and the accumulator and writes to the accumulator.
    BitwiseExclusiveOr,

    /// INC: Increments a memory location. Cannot be used for incrementing the accumulator; ADC or SBC must be used instead. NOTE: Does NOT affect the carry nor overflow flags.
    IncrementMemory,

    /// LDA: Loads a memory value into the accumulator.
    LoadAcc,

    /// LDA: Loads a memory value into the X register.
    LoadX,

    /// LDA: Loads a memory value into the Y register.
    LoadY,

    /// LSR: Shifts all the bits in the accumulator or a memory value one position to the right. Lowest bit is shifted into the carry flag.
    LogicalShiftRight,

    /// ORA: Bitwise ORs the accumulator and a memory value.
    BitwiseOr,

    /// ROL: Shifts a memory value or the accumulator to the left. The highest bit is rotated into the carry flag, and the carry flag is rotated into the lowest bit.
    RotateLeft,

    /// ROL: Shifts a memory value or the accumulator to the right. The lowest bit is rotated into the carry flag, and the carry flag is rotated into the highest bit.
    RotateRight,

    /// SBC: Subtracts a memory value and the bitwise negation of the carry flag from the accumulator.
    SubtractWithCarry,

    /// STA: Stores the accumulator value into memory.
    StoreAcc,

    /// STX: Stores the X register value into memory.
    StoreX,

    /// STY: Stores the Y register value into memory.
    StoreY,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BranchInstruction {
    /// BCC: Add value to program counter if carry flag is clear.
    BranchIfCarryClear,

    /// BCS: Add value to program counter if carry flag is set.
    BranchIfCarrySet,

    /// BEQ: Add value to program counter if zero flag is set.
    BranchIfEqual,

    /// BMI: Add value to program counter if negative flag is set.
    BranchIfMinus,

    /// BEQ: Add value to program counter if zero flag is clear.
    BranchIfNotEqual,

    /// BPL: Add value to program counter if negative flag is clear.
    BranchIfPlus,

    /// BVC: Add value to program counter if overflow flag is clear.
    BranchIfOverflowClear,

    /// BVC: Add value to program counter if overflow flag is set.
    BranchIfOverflowSet,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Target {}

/// 6502 Addressing Modes. Defines different possible formats for fetching instructions arguments. More info about this can be found at https://www.nesdev.org/obelisk-6502-guide/addressing.html.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryTarget {
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

impl MemoryTarget {
    pub const fn size(&self) -> usize {
        use MemoryTarget as B;
        match self {
            B::Accumulator => 0,
            B::Immediate(_)
            | B::ZeroPage(_)
            | B::ZeroPageX(_)
            | B::ZeroPageY(_)
            | B::IndirectX(_)
            | B::IndirectY(_) => 1,
            B::Absolute(_) | B::AbsoluteX(_) | B::AbsoluteY(_) => 2,
        }
    }

    pub const fn cycles(&self) -> i128 {
        use MemoryTarget as B;
        match self {
            B::Accumulator => 1,
            B::Immediate(_) => 1,
            B::ZeroPage(_) => 2,
            B::ZeroPageX(_) => 3,
            B::ZeroPageY(_) => 3,
            B::Absolute(_) => 3,
            B::AbsoluteX(_) => 3,
            B::AbsoluteY(_) => 3,
            B::IndirectX(_) => 5,
            B::IndirectY(_) => 4,
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
            .ok_or_else(|| DecodeError::NeedsMoreData(N - self.len()))?;
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
            .ok_or_else(|| DecodeError::NeedsMoreData(n - self.len()))?;
        Ok((data, taken_bytes))
    }
}

macro_rules! implied_instruction {
    ($instruction:ident, $data:ident) => {{
        (
            $data,
            Instruction::Implied(ImpliedInstruction::$instruction),
        )
    }};
}

macro_rules! byte_instruction {
    ($instruction:ident, $opcode:ident, $data:ident) => {{
        let (data, location) =
            Instruction::location($data, ByteLocationType::from_opcode($opcode))?;

        (
            data,
            Instruction::Memory {
                instruction: MemoryInstruction::$instruction,
                target: location,
            },
        )
    }};
}

macro_rules! branch_instruction {
    ($instruction:ident, $data:ident) => {{
        let (data, offset) = $data.take_one()?;
        (
            data,
            Instruction::Branch {
                instruction: BranchInstruction::$instruction,
                offset: offset as i8,
            },
        )
    }};
}

#[derive(PartialEq, Eq, Debug, Error)]
pub enum DecodeError {
    #[error("invalid opcode: ${0:#X}")]
    InvalidOpcode(u8),
    #[error("needs more data: {0} bytes")]
    NeedsMoreData(usize),
}

impl Instruction {
    pub fn decode(data: &[u8]) -> Result<(&[u8], Instruction), DecodeError> {
        use Instruction as I;
        let (data, opcode) = data.take_one()?;
        let (data, instruction) = match opcode {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                byte_instruction!(AddWithCarry, opcode, data)
            }
            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                byte_instruction!(BitwiseAnd, opcode, data)
            }
            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
                byte_instruction!(ArithmeticShiftLeft, opcode, data)
            }
            0x90 => branch_instruction!(BranchIfCarryClear, data),
            0xB0 => branch_instruction!(BranchIfCarrySet, data),
            0xF0 => branch_instruction!(BranchIfEqual, data),
            0x24 | 0x2C => byte_instruction!(BitTest, opcode, data),
            0x30 => branch_instruction!(BranchIfMinus, data),
            0xD0 => branch_instruction!(BranchIfNotEqual, data),
            0x10 => branch_instruction!(BranchIfPlus, data),
            0x00 => {
                // The return address that is pushed to the stack skips the following byte (current program counter + 2), so we shift data by 1.
                let (data, _) = data.take_one()?;
                (data, I::Implied(ImpliedInstruction::Break))
            }
            0x50 => branch_instruction!(BranchIfOverflowClear, data),
            0x70 => branch_instruction!(BranchIfOverflowSet, data),
            0x18 => implied_instruction!(ClearCarry, data),
            0xD8 => implied_instruction!(ClearDecimal, data),
            0x58 => implied_instruction!(ClearInterruptDisable, data),
            0xB8 => implied_instruction!(ClearInterruptDisable, data),
            0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => {
                byte_instruction!(CompareAcc, opcode, data)
            }
            0xE0 | 0xE4 | 0xEC => byte_instruction!(CompareX, opcode, data),
            0xC0 | 0xC4 | 0xCC => byte_instruction!(CompareY, opcode, data),
            0xC6 | 0xD6 | 0xCE | 0xDE => {
                byte_instruction!(DecrementMemory, opcode, data)
            }
            0xCA => implied_instruction!(DecrementX, data),
            0x88 => implied_instruction!(DecrementY, data),

            0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
                byte_instruction!(BitwiseExclusiveOr, opcode, data)
            }

            0xE6 | 0xF6 | 0xEE | 0xFE => {
                byte_instruction!(IncrementMemory, opcode, data)
            }

            0xE8 => implied_instruction!(IncrementX, data),
            0xC8 => implied_instruction!(IncrementY, data),

            0x4C | 0x6C => {
                let (data, [low, high]) = data.take()?;
                let addr = ((high as Address) << 8) | (low as Address);
                (
                    data,
                    I::Jump(match opcode {
                        0x4C => JumpAddress::Absolute(addr),
                        0x6C => JumpAddress::Indirect(addr),
                        _ => unreachable!(),
                    }),
                )
            }
            0x20 => {
                let (data, [low, high]) = data.take()?;
                let addr = ((high as Address) << 8) | (low as Address);
                (data, I::JumpToSubroutine(addr))
            }
            0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                byte_instruction!(LoadAcc, opcode, data)
            }
            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
                byte_instruction!(LoadX, opcode, data)
            }
            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
                byte_instruction!(LoadY, opcode, data)
            }
            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => {
                byte_instruction!(LogicalShiftRight, opcode, data)
            }
            0xEA => implied_instruction!(NoOperation, data),

            0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
                byte_instruction!(BitwiseOr, opcode, data)
            }
            0x48 => implied_instruction!(PushAcc, data),
            0x08 => implied_instruction!(PushProcessorStatus, data),
            0x68 => implied_instruction!(PullAcc, data),
            0x28 => implied_instruction!(PullProcessorStatus, data),
            0x2A | 0x26 | 0x36 | 0x2E | 0x3E => {
                byte_instruction!(RotateLeft, opcode, data)
            }
            0x6A | 0x66 | 0x76 | 0x6E | 0x7E => {
                byte_instruction!(RotateRight, opcode, data)
            }
            0x40 => implied_instruction!(ReturnFromInterrupt, data),
            0x60 => implied_instruction!(ReturnFromSubroutine, data),
            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
                byte_instruction!(SubtractWithCarry, opcode, data)
            }
            0x38 => implied_instruction!(SetCarry, data),
            0xF8 => implied_instruction!(SetDecimal, data),
            0x78 => implied_instruction!(SetInterruptDisable, data),

            0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
                byte_instruction!(StoreAcc, opcode, data)
            }
            0x86 | 0x96 | 0x8E => byte_instruction!(StoreX, opcode, data),
            0x84 | 0x94 | 0x8C => byte_instruction!(StoreY, opcode, data),
            0xAA => implied_instruction!(TransferAccToX, data),
            0xA8 => implied_instruction!(TransferAccToY, data),
            0xBA => implied_instruction!(TransferStackPointerToX, data),
            0x8A => implied_instruction!(TransferXToAcc, data),
            0x9A => implied_instruction!(TransferXToStackPointer, data),
            0x98 => implied_instruction!(TransferYToAcc, data),

            _ => return Err(DecodeError::InvalidOpcode(opcode)),
        };

        Ok((data, instruction))
    }

    fn location(
        data: &[u8],
        byte_type: ByteLocationType,
    ) -> Result<(&[u8], MemoryTarget), DecodeError> {
        use ByteLocationType as BT;
        use MemoryTarget as B;
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
    use BranchInstruction as BI;
    use Instruction as I;
    use MemoryInstruction as MI;
    use MemoryTarget as B;

    #[test]
    fn test_decode() {
        let data = [0x16, 0x55, 0x30, -0x12i8 as u8, 0x3D, 0xAD, 0xDE].as_slice();
        assert_eq!(
            I::decode(data),
            Ok((
                &data[2..],
                I::Memory {
                    instruction: MI::ArithmeticShiftLeft,
                    target: B::ZeroPageX(0x55)
                }
            ))
        );

        assert_eq!(
            I::decode(&data[2..]),
            Ok((
                &data[4..],
                I::Branch {
                    instruction: BI::BranchIfMinus,
                    offset: -0x12,
                }
            ))
        );

        assert_eq!(
            I::decode(&data[4..]),
            Ok((
                &data[7..],
                I::Memory {
                    instruction: MI::BitwiseAnd,
                    target: B::AbsoluteX(0xDEAD)
                }
            ))
        );

        assert_eq!(I::decode(&data[7..]), Err(DecodeError::NeedsMoreData(1)));
    }
}
