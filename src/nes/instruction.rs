use super::{Address, ShortAddress};

/// Info about 6502 instructions have been taken from https://www.nesdev.org/wiki/Instruction_reference.
#[derive(Debug)]
pub enum Instruction {
    // Official instructions
    /// ADC: Adds the carry flag and a memory value to the accumulator.
    AddWithCarry(MemoryByte),

    /// AND: Performs bitwise AND on accumulator with a memory value.
    BitwiseAnd(MemoryByte),

    /// ASL: Shifts all bits in value left, moving the value of each bit into the next bit. Bit 7 is shifted into the carry flag, and bit 0 is cleared.
    ArithmeticShiftLeft(MemoryByte),

    /// BCC: Add value to program counter if carry flag is clear.
    BranchIfCarryClear(i8),

    /// BCS: Add value to program counter if carry flag is set.
    BranchIfCarrySet(i8),

    /// BEQ: Add value to program counter if zero flag is set.
    BranchIfEqual(i8),

    /// BIT: Modfies flags, but does not change memory or registers. Zero flag is set if accumulator & memory value != 0. Bits 7 and 6 are loaded directly into the negative and overflow flags.
    BitTest(MemoryByte),

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
#[derive(Debug, Clone, Copy)]
pub enum MemoryByte {
    /// The value in the A register.
    Accumulator,

    /// Allows the programmer to directly specify an 8-bit value. Indicated by a `#` symbol followed by the value (e.g. #F8).
    Immediate(u8),

    /// A memory access limited to the first 256 bytes of memory (i.e. $0000 to $00FF).
    ZeroPage(ShortAddress),

    /// Same as ZeroPage, but the X register is added to the address beforehand. If the addition is greater than #FF, it wraps around.
    ZeroPageX(ShortAddress),

    /// Same as ZeroPageX, but using the Y register instead. Can only be used with the LDX and STX instructions.
    ZeroPageY(ShortAddress),

    /// A memory access using a full 16-bit address.
    Absolute(Address),

    /// Same as Absolute, but the X register is added to the address beforehand.
    AbsoluteX(Address),

    /// Same as Absolute, but the Y register is added to the address beforehand.
    AbsoluteY(Address),
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryWord {
    /// Reads the two bytes at address (little-endian).
    Indirect(Address),

    /// Reads the two bytes held in zero page at address + X (little-endian). Wraps around if addition is greater than $FF.
    IndirectX(ShortAddress),

    /// Same as IndirectX, but using the Y register instead.
    IndirectY(ShortAddress),
}
