use instruction::{Instruction, MemoryValue};

use crate::bits::get_bit;

pub type Register = u8;
pub type Address = u16;
pub type ShortAddress = u8;

mod instruction;

struct Nes {
    cpu: Cpu,
    memory: [u8; 2048],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Interrupt {
    Software,
    Hardware,
}

#[derive(Clone, Copy, Debug)]
struct Cpu {
    /// General purpose registers
    acc: Register,
    /// Index register X, the main register for accessing data with indexes.
    x: Register,
    y: Register,
    /// Status register
    p: Register,
    /// Stack pointer
    sp: Register,
    /// Instructions pointer
    pc: Address,
    status: Status,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
            interrupt_disable: false,
            decimal_mode: false,
            interrupt: Interrupt::Software,
            unused: true,
            overflow: false,
            negative: false,
        }
    }
}

impl Nes {
    fn execute(&mut self, instruction: Instruction) {
        use Instruction as I;
        match instruction {
            I::AddWithCarry(v) => self
                .cpu
                .add_acc(self.value(v) as u8 + self.cpu.status.carry as u8),
            I::BitwiseAnd(v) => todo!(),
            I::ArithmeticShiftLeft(v) => todo!(),
            I::BranchIfCarryClear(_) => todo!(),
            I::BranchIfCarrySet(_) => todo!(),
            I::BranchIfEqual(_) => todo!(),
            I::BitTest(v) => todo!(),
            I::BranchIfMinus(_) => todo!(),
            I::BranchIfNotEqual(_) => todo!(),
            I::BranchIfPlus(_) => todo!(),
            I::Break => todo!(),
            I::BranchIfOverflowClear(_) => todo!(),
            I::BranchIfOverflowSet(_) => todo!(),
            I::ClearCarry => todo!(),
            I::ClearDecimal => todo!(),
        }
    }

    fn value(&self, val: MemoryValue) -> u16 {
        use MemoryValue as M;

        if matches!(val, M::Accumulator) {
            return self.cpu.acc as u16;
        }

        let addr = match val {
            M::ZeroPage(addr) => Some(addr as Address),
            M::ZeroPageX(addr) => Some(addr.wrapping_add(self.cpu.x) as Address),
            M::ZeroPageY(addr) => Some(addr.wrapping_add(self.cpu.y) as Address),
            M::Absolute(addr) => Some(addr),
            M::AbsoluteX(addr) => Some(addr.wrapping_add(self.cpu.x as Address)),
            M::AbsoluteY(addr) => Some(addr.wrapping_add(self.cpu.y as Address)),
            _ => None,
        };

        if let Some(addr) = addr {
            return self.memory[addr as usize] as u16;
        }

        let addr = match val {
            M::Indirect(addr) => addr,
            M::IndirectX(addr) => addr.wrapping_add(self.cpu.x) as Address,
            M::IndirectY(addr) => addr.wrapping_add(self.cpu.y) as Address,
            _ => unreachable!("all cases should have been handled at this point"),
        };

        self.get_le_u16(addr)
    }

    fn get_le_u16(&self, addr: Address) -> u16 {
        let addr = addr as usize;
        ((self.memory[addr + 1] as u16) << 8) | self.memory[addr] as u16
    }
}

impl Cpu {
    fn set_acc(&mut self, value: u8) {
        self.acc = value;
        let zero = self.acc == 0;
        let negative = (self.acc >> 7) != 0;
        self.status = Status {
            zero,
            negative,
            ..self.status
        };
    }

    fn add_acc(&mut self, value: u8) {
        let new_acc = self.acc.wrapping_add(value);
        let carry = self.acc as u16 + value as u16 > u8::MAX.into();
        let overflow = (self.acc <= u8::MAX / 2) != (new_acc <= u8::MAX / 2);
        self.status = Status {
            carry,
            overflow,
            ..self.status
        };
        self.set_acc(new_acc);
    }

    fn sub_acc(&mut self, value: u8) {
        let new_acc = self.acc.wrapping_sub(value);
        let carry = self.acc < value;
        let overflow = (self.acc <= u8::MAX / 2) != (new_acc <= u8::MAX / 2);
        self.status = Status {
            carry,
            overflow,
            ..self.status
        };
        self.set_acc(new_acc);
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            acc: 0,
            x: 0,
            y: 0,
            p: 0,
            sp: 0,
            pc: 0,
            status: Status::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_acc() {
        let mut cpu = Cpu::default();

        cpu.add_acc(150);

        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(cpu.status.negative);

        cpu.add_acc(106);
        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);
    }
}
