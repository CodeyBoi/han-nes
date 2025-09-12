use std::{
    ops::{Index, IndexMut},
    path::Path,
};

use instruction::{ByteLocation, Instruction, WordLocation};

use crate::bits::BitAddressable;

pub type Register = u8;
pub type Address = u16;
pub type ShortAddress = u8;

mod instruction;

pub struct Nes {
    cpu: Cpu,
    memory: [u8; MEMORY_SIZE],
}

const MEMORY_SIZE: usize = 0x800;
const STACK_BASE_ADDR: usize = 0x100;
const STACK_SIZE: usize = 0x100;

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
    /// Stack pointer
    stack_pointer: Register,
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
            interrupt_disable: false,
            decimal_mode: false,
            interrupt: Interrupt::Software,
            unused: true,
            overflow: false,
            negative: false,
        }
    }
}

impl Default for Nes {
    fn default() -> Self {
        Self {
            cpu: Cpu::default(),
            memory: [0; MEMORY_SIZE],
        }
    }
}

impl Nes {
    pub fn load_rom<P>(&mut self, _filepath: &P)
    where
        P: AsRef<Path> + ?Sized,
    {
        todo!()
    }

    fn execute(&mut self, instruction: Instruction) {
        use Instruction as I;
        match instruction {
            I::AddWithCarry(b) => {
                self.cpu.acc_add(self.read(b) + self.cpu.status.carry as u8);
            }
            I::BitwiseAnd(b) => {
                self.cpu.acc_set(self.cpu.acc & self.read(b));
            }
            I::ArithmeticShiftLeft(b) => {
                let value = self.read(b);
                let shifted = value << 1;
                self.cpu.status = Status {
                    carry: value.bit(7),
                    zero: shifted == 0,
                    negative: shifted.bit(7),
                    ..self.cpu.status
                };
                self.write(b, value);
                self.write(b, shifted);
            }
            I::BranchIfCarryClear(offset) => {
                self.branch(offset, !self.cpu.status.carry);
            }
            I::BranchIfCarrySet(offset) => {
                self.branch(offset, self.cpu.status.carry);
            }
            I::BranchIfEqual(offset) => {
                self.branch(offset, self.cpu.status.zero);
            }
            I::BitTest(b) => {
                let value = self.read(b);
                self.cpu.status = Status {
                    zero: (self.cpu.acc & value) == 0,
                    overflow: value.bit(6),
                    negative: value.bit(7),
                    ..self.cpu.status
                };
            }
            I::BranchIfMinus(offset) => self.branch(offset, self.cpu.status.negative),
            I::BranchIfNotEqual(offset) => self.branch(offset, !self.cpu.status.zero),
            I::BranchIfPlus(offset) => self.branch(offset, !self.cpu.status.negative),
            I::Break => {
                self.push_u16(self.cpu.pc + 2);
                let pushed_status = Status {
                    interrupt: Interrupt::Software,
                    ..self.cpu.status
                };
                self.push(pushed_status.into());
                self.cpu.status.interrupt_disable = true;
                self.cpu.pc = 0xFFFE;
            }
            I::BranchIfOverflowClear(offset) => self.branch(offset, !self.cpu.status.overflow),
            I::BranchIfOverflowSet(offset) => self.branch(offset, self.cpu.status.overflow),
            I::ClearCarry => self.cpu.status.carry = false,
            I::ClearDecimal => self.cpu.status.decimal_mode = false,
        }
    }

    fn push(&mut self, value: u8) {
        // 6502 uses an empty stack, meaning the stack pointer points to the element where the next value will be stored.
        if self.cpu.stack_pointer == 0 {
            eprintln!("WARNING: stack full, next push will overflow!");
        }
        self.memory[STACK_BASE_ADDR + self.cpu.stack_pointer as usize] = value;
        self.cpu.stack_pointer = self.cpu.stack_pointer.wrapping_add(1);
    }

    fn push_u16(&mut self, value: u16) {
        let (high, low) = (value.bits(8..16) as u8, value.bits(0..8) as u8);
        self.push(low);
        self.push(high);
    }

    /// Fetches the byte located at `location`. Used for fetching the actual argument values for instructions.
    fn read(&self, location: ByteLocation) -> u8 {
        use ByteLocation as B;
        let addr = match location {
            B::Accumulator => return self.cpu.acc,
            B::Immediate(v) => return v,
            B::ZeroPage(addr) => addr as Address,
            B::ZeroPageX(addr) => addr.wrapping_add(self.cpu.x) as Address,
            B::ZeroPageY(addr) => addr.wrapping_add(self.cpu.y) as Address,
            B::Absolute(addr) => addr,
            B::AbsoluteX(addr) => addr.wrapping_add(self.cpu.x as Address),
            B::AbsoluteY(addr) => addr.wrapping_add(self.cpu.y as Address),
            B::IndirectX(addr) => self.get_le_u16(addr.wrapping_add(self.cpu.x) as Address),
            B::IndirectY(addr) => self.get_le_u16(addr.wrapping_add(self.cpu.y) as Address),
        };
        self.memory[addr as usize]
    }

    /// Modifies the byte located at `location`. Used in instructions which modify their arguments (e.g. ASL - Arithmetic Left Shift).
    fn write(&mut self, location: ByteLocation, value: u8) {
        use ByteLocation as B;
        let addr = match location {
            B::Accumulator => {
                self.cpu.acc_set(value);
                return;
            }
            B::Immediate(v) => panic!("tried to write to immediate value #{:X}", v),
            B::ZeroPage(addr) => addr as Address,
            B::ZeroPageX(addr) => addr.wrapping_add(self.cpu.x) as Address,
            B::ZeroPageY(addr) => addr.wrapping_add(self.cpu.y) as Address,
            B::Absolute(addr) => addr,
            B::AbsoluteX(addr) => addr.wrapping_add(self.cpu.x as Address),
            B::AbsoluteY(addr) => addr.wrapping_add(self.cpu.y as Address),
            B::IndirectX(addr) => self.get_le_u16(addr.wrapping_add(self.cpu.x) as Address),
            B::IndirectY(addr) => self.get_le_u16(addr.wrapping_add(self.cpu.y) as Address),
        };
        self.memory[addr as usize] = value;
    }

    fn read_word(&self, location: WordLocation) -> u16 {
        use WordLocation as W;
        let addr_of_addr = match location {
            W::Indirect(a) => a,
        };
        let addr = self.get_le_u16(addr_of_addr);
        self.get_le_u16(addr)
    }

    fn get_le_u16(&self, addr: Address) -> u16 {
        let addr = addr as usize;
        ((self.memory[addr + 1] as u16) << 8) | self.memory[addr] as u16
    }

    fn branch(&mut self, offset: i8, condition: bool) {
        if condition {
            self.cpu.pc = self.cpu.pc.wrapping_add_signed(offset as i16);
        }
    }
}

impl Cpu {
    fn acc_set(&mut self, value: u8) {
        let zero = value == 0;
        let negative = value.bit(7);
        self.status = Status {
            zero,
            negative,
            ..self.status
        };
        self.acc = value;
    }

    fn acc_add(&mut self, value: u8) {
        let (new_acc, carry) = self.acc.overflowing_add(value);
        let (_, overflow) = (self.acc as i8).overflowing_add_unsigned(value);
        self.status = Status {
            carry,
            overflow,
            ..self.status
        };
        self.acc_set(new_acc);
    }

    fn acc_sub(&mut self, value: u8) {
        let (new_acc, carry) = self.acc.overflowing_sub(value);
        let (_, overflow) = (self.acc as i8).overflowing_sub_unsigned(value);
        self.status = Status {
            carry,
            overflow,
            ..self.status
        };
        self.acc_set(new_acc);
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            acc: 0,
            x: 0,
            y: 0,
            stack_pointer: 0xFF,
            pc: 0,
            status: Status::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_acc_add() {
        let mut cpu = Cpu::default();

        cpu.acc_set(0);
        cpu.acc_add(150);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(cpu.status.negative);

        cpu.acc_add(106);
        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(250);
        cpu.acc_add(10);
        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(120);
        cpu.acc_add(136);
        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(i8::MAX as u8);
        cpu.acc_add(1);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(cpu.status.negative);
    }

    #[test]
    fn test_acc_sub() {
        let mut cpu = Cpu::default();

        cpu.acc_set(0);
        cpu.acc_sub(1);
        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(cpu.status.negative);

        cpu.acc_set(5);
        cpu.acc_sub(10);
        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(cpu.status.negative);

        assert_eq!(-1i8 as u8, 0xff);
        cpu.acc_set(-1i8 as u8);
        cpu.acc_sub(150);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(120);
        cpu.acc_sub(120);
        assert!(!cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(150);
        cpu.acc_sub(150);
        assert!(!cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(!cpu.status.negative);

        cpu.acc_set(120);
        cpu.acc_sub(121);
        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(cpu.status.negative);

        cpu.acc_set(150);
        cpu.acc_sub(151);
        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(cpu.status.negative);

        cpu.acc_set(i8::MIN as u8);
        cpu.acc_sub(1);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.overflow);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_add_with_carry() {
        use ByteLocation as B;
        use Instruction as I;
        let mut nes = Nes::default();
        let addr = 0x10;
        let mem_value = 0x8;

        nes.write(B::ZeroPage(addr), mem_value);
        assert_eq!(nes.read(B::ZeroPage(addr)), mem_value);

        nes.cpu.acc_set(0);
        nes.execute(I::AddWithCarry(B::Immediate(200)));
        assert_eq!(nes.read(B::Accumulator), 200);
        assert!(!nes.cpu.status.carry);

        nes.execute(I::AddWithCarry(B::Immediate(100)));
        assert_eq!(nes.read(B::Accumulator), 200u8.wrapping_add(100));
        assert!(nes.cpu.status.carry);

        let old_acc = nes.read(B::Accumulator);
        nes.execute(I::AddWithCarry(B::ZeroPage(addr)));
        assert_eq!(nes.read(B::Accumulator), old_acc + mem_value + 1);
        assert!(!nes.cpu.status.carry);
    }
}
