use crate::bits::get_bit;

pub type Register = u8;
pub type Address = u16;
pub type ShortAddress = u8;

mod instruction;

struct Nes {
    cpu: Cpu,
    memory: [u8; 2048],
}

enum Interrupt {
    Software,
    Hardware,
}

struct Status {
    /// Used in additions, subtractions, comparisons and bit rotations. In additions and subtractions it acts as a 9th bit. Comparisons are a special case of subtraction, as they assume carry flag set and decimal flag clear, and do not save the result anywhere. For bit rotations, the bit that is rotated off is stored in the carry flag.
    carry: bool,
    /// Is set if an arithmetic register is loaded with the value 0. Will behave differently in decimal mode.
    zero: bool,
    /// Prevents the CPU from jumping to the IRQ handler vector ($FFFE) whenever the hardware line -IRQ is active. It is automatically set after taking an interrupt.
    interrupt_disable: bool,
    /// Selects the (Binary Coded) Decimal mode for addition and subtraction. Is most often cleared.
    decimal_mode: bool,
    /// Distinguishes software interrupts (BRK) from hardware interrupts (IRQ or NMI).
    interrupt: Interrupt,
    /// Is always true.
    unused: bool,
    /// After a binary addition or subtraction this flag will be set on a sign overflow, otherwise cleared. Will not be set as expected in decimal mode.
    overflow: bool,
    /// This flag will be set after any arithmetic operation. Generally, the flag will be copied from the topmost bit of the result (as that's the sign bit). Will not be set as expected in decimal mode.
    negative: bool,
}

impl From<u8> for Status {
    fn from(value: u8) -> Self {
        Self {
            carry: get_bit(value, 0),
            zero: get_bit(value, 1),
            interrupt_disable: get_bit(value, 2),
            decimal_mode: get_bit(value, 3),
            interrupt: if get_bit(value, 4) {
                Interrupt::Software
            } else {
                Interrupt::Hardware
            },
            unused: get_bit(value, 5),
            overflow: get_bit(value, 6),
            negative: get_bit(value, 7),
        }
    }
}

impl From<Status> for u8 {
    fn from(value: Status) -> Self {}
}

struct Cpu {
    /// General purpose registers
    accumulator: Register,
    /// Index register X, the main register for accessing data with indexes.
    x: Register,
    y: Register,
    /// Status register
    p: Register,
    /// Stack pointer
    sp: Register,
    /// Instructions pointer
    pc: Address,
}

impl Nes {
    fn execute() {}
}
