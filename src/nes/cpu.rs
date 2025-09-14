use crate::bits::BitAddressable as _;

pub type Register = u8;
pub type Address = u16;
pub type ShortAddress = u8;

#[derive(Clone, Debug)]
pub struct Cpu {
    /// General arithmetic register A (accumulator)
    pub acc: Register,
    /// Index register X, the main register for accessing data with indexes.
    pub x: Register,
    /// Index register Y, like X but less used.
    pub y: Register,
    /// Stack pointer. The 6502 uses an empty stack which grows downwards, meaning the stack pointer points to the first empty slot. It is therefore initialzied to $FF.
    pub stack_pointer: Register,
    /// Program counter, points to the current instruction.
    pub pc: Address,
    /// A collection of status and arithmetic flags.
    pub status: Status,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Status {
    /// Used in additions, subtractions, comparisons and bit rotations. In additions and subtractions it acts as a 9th bit. Comparisons are a special case of subtraction, as they assume carry flag set and decimal flag clear, and do not save the result anywhere. For bit rotations, the bit that is rotated off is stored in the carry flag.
    pub carry: bool,
    /// Is set if an arithmetic register is loaded with the value 0. Will behave differently in decimal mode.
    pub zero: bool,
    /// Prevents the CPU from jumping to the IRQ handler vector ($FFFE) whenever the hardware line -IRQ is active. It is automatically set after taking an interrupt.
    pub interrupt_disable: bool,
    /// Selects the (Binary Coded) Decimal mode for addition and subtraction. Is most often cleared.
    pub decimal_mode: bool,
    /// Distinguishes software interrupts (BRK) from hardware interrupts (IRQ or NMI).
    pub interrupt: Interrupt,
    /// Is always true.
    pub unused: bool,
    /// After a binary addition or subtraction this flag will be set on a sign overflow, otherwise cleared. Will not be set as expected in decimal mode.
    pub overflow: bool,
    /// This flag will be set after any arithmetic operation. Generally, the flag will be copied from the topmost bit of the result (as that's the sign bit). Will not be set as expected in decimal mode.
    pub negative: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Interrupt {
    Software,
    Hardware,
}

impl From<u8> for Status {
    fn from(value: u8) -> Self {
        Self {
            carry: value.bit(0),
            zero: value.bit(1),
            interrupt_disable: value.bit(2),
            decimal_mode: value.bit(3),
            interrupt: if value.bit(4) {
                Interrupt::Software
            } else {
                Interrupt::Hardware
            },
            unused: value.bit(5),
            overflow: value.bit(6),
            negative: value.bit(7),
        }
    }
}

impl From<Status> for u8 {
    fn from(value: Status) -> Self {
        [
            value.carry,
            value.zero,
            value.interrupt_disable,
            value.decimal_mode,
            match value.interrupt {
                Interrupt::Software => true,
                Interrupt::Hardware => false,
            },
            value.unused,
            value.overflow,
            value.negative,
        ]
        .iter()
        .enumerate()
        .fold(0, |acc, (i, v)| acc | ((*v as u8) << i))
    }
}

impl Default for Status {
    fn default() -> Self {
        Self {
            carry: false,
            zero: false,
            interrupt_disable: true,
            decimal_mode: false,
            interrupt: Interrupt::Software,
            unused: true,
            overflow: false,
            negative: false,
        }
    }
}
