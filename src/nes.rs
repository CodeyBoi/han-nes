use std::io::Write;
use std::{
    fs::{self, File},
    path::Path,
    thread,
    time::{Duration, Instant},
};

use cpu::{Address, Cpu, Interrupt, Status};
use instruction::{ByteLocation, Instruction, JumpAddress, Take as _};
use memory::{MemoryMap, STACK_BASE};

use crate::bits::BitAddressable;

mod cpu;
mod instruction;
mod memory;

pub struct Nes {
    cpu: Cpu,
    memory: MemoryMap,
    is_running: bool,
    cycle: u128,
    should_log: bool,
    executed_instructions: Vec<Instruction>,
}

// TODO: Create struct with different hardware values for the different regions so you can load them dynamically.
const CLOCK_HZ: usize = 1789773; // 1662607 for PAL

pub const MAGIC_TAG: &[u8; 4] = &[b'N', b'E', b'S', 0x1A];

impl Default for Nes {
    fn default() -> Self {
        Self {
            cpu: Cpu::default(),
            memory: MemoryMap::new(),
            is_running: true,
            cycle: 0,
            should_log: true,
            executed_instructions: Vec::new(),
        }
    }
}

pub enum LoadError {
    ReadError,
    NeedsMoreData(usize),
    InvalidMagic([u8; 4]),
}

impl Nes {
    pub fn load_rom<P>(&mut self, filepath: &P) -> Result<(), LoadError>
    where
        P: AsRef<Path> + ?Sized,
    {
        let data = fs::read(filepath).or(Err(LoadError::ReadError))?;

        let (data, magic) = data
            .as_slice()
            .take()
            .map_err(|_| LoadError::NeedsMoreData(4 - data.len()))?;
        if &magic != MAGIC_TAG {
            return Err(LoadError::InvalidMagic(magic));
        }

        let (data, prg_rom_size) = data.take_one().or(Err(LoadError::NeedsMoreData(1)))?;
        let (data, _chr_rom_size) = data.take_one().or(Err(LoadError::NeedsMoreData(1)))?;

        let (data, flags) = data
            .take::<10>()
            .map_err(|_| LoadError::NeedsMoreData(10 - data.len()))?;

        let has_trainer = flags[0].bit(2);

        let (data, _trainer) = if has_trainer {
            data.take::<512>()
                .map_err(|_| LoadError::NeedsMoreData(512 - data.len()))?
        } else {
            (data, [0x0; 512])
        };

        let prg_rom_bytes = prg_rom_size as usize * (0x1 << 14);
        let (data, prg_rom) = data
            .take_n(prg_rom_bytes)
            .map_err(|_| LoadError::NeedsMoreData(prg_rom_bytes - data.len()))?;

        self.memory.load_program_rom(prg_rom);

        self.cpu.pc = self.get_le_u16(0xFFFC);

        Ok(())
    }

    pub fn run(&mut self) {
        let now = Instant::now();
        let duration_per_cycle = Duration::from_nanos(1_000_000_000 / CLOCK_HZ as u64);
        let mut next_cycle = now;

        let mut logfile = if self.should_log {
            File::create("log").ok()
        } else {
            None
        };

        while self.is_running {
            let now = Instant::now();
            let start = Instant::now();

            if now >= next_cycle {
                let data = self.memory.slice_from(self.cpu.pc);
                let data_start = data.as_ptr() as usize;
                let (new_data, instruction) = match Instruction::decode(data) {
                    Ok(v) => v,
                    Err(e) => panic!("{:#X?}", e),
                };
                let data_end = new_data.as_ptr() as usize;

                // TODO: Add an actual cycle count
                let cycles = 7;

                if let Some(ref mut logfile) = logfile {
                    writeln!(logfile, "{:?}", instruction).expect("failed writing to logfile");
                }
                self.execute(instruction);

                next_cycle += duration_per_cycle * cycles;
                self.cycle += cycles as u128;
                self.cpu.pc += (data_end - data_start) as u16;
            }

            thread::sleep(duration_per_cycle.saturating_sub(start.elapsed()));
        }
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
                self.cpu.status.carry = value.bit(7);
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
            I::ClearInterruptDisable => self.cpu.status.interrupt_disable = false,
            I::ClearOverflow => self.cpu.status.overflow = false,
            I::CompareAcc(b) => self.cpu.status = self.compare(self.cpu.acc, self.read(b)),
            I::CompareX(b) => self.cpu.status = self.compare(self.cpu.x, self.read(b)),
            I::CompareY(b) => self.cpu.status = self.compare(self.cpu.y, self.read(b)),
            I::DecrementMemory(b) => self.write(b, self.read(b).wrapping_sub(1)),
            I::DecrementX => self.cpu.x_set(self.cpu.x.wrapping_sub(1)),
            I::DecrementY => self.cpu.y_set(self.cpu.y.wrapping_sub(1)),
            I::BitwiseExclusiveOr(b) => self.cpu.acc_set(self.cpu.acc ^ self.read(b)),
            I::IncrementMemory(b) => self.write(b, self.read(b).wrapping_add(1)),
            I::IncrementX => self.cpu.x_set(self.cpu.x.wrapping_add(1)),
            I::IncrementY => self.cpu.y_set(self.cpu.y.wrapping_add(1)),
            I::Jump(jump_address) => {
                let addr = match jump_address {
                    JumpAddress::Absolute(addr) => addr,
                    JumpAddress::Indirect(addr_of_addr) => {
                        let low = self.memory[addr_of_addr] as u16;
                        // Cpu has a bug where if the start of this 2-byte variable is at an address ending in $FF it reads the second byte from the start of the page (each page is $100 bytes). We emulate this.
                        let high_addr = if (addr_of_addr % 0x100) == 0xFF {
                            addr_of_addr + 1 - 0x100
                        } else {
                            addr_of_addr + 1
                        };
                        let high = self.memory[high_addr] as u16;
                        (high << 8) | low
                    }
                };
                self.cpu.pc = addr;
            }
            I::JumpToSubroutine(addr) => match addr {
                ByteLocation::Absolute(addr) => {
                    self.push_u16(self.cpu.pc + 2);
                    self.cpu.pc = addr;
                }
                _ => unreachable!("invalid bytelocation for JumpToSubroutine"),
            },
            I::LoadAcc(b) => self.cpu.acc_set(self.read(b)),
            I::LoadX(b) => self.cpu.x_set(self.read(b)),
            I::LoadY(b) => self.cpu.y_set(self.read(b)),
            I::LogicalShiftRight(b) => {
                let value = self.read(b);
                self.cpu.status.carry = value.bit(0);
                self.write(b, value >> 1);
            }
            I::NoOperation => {}
            I::BitwiseOr(b) => self.cpu.acc_set(self.cpu.acc | self.read(b)),
            I::PushAcc => self.push(self.cpu.acc),
            I::PushProcessorStatus => self.push(self.cpu.status.into()),
            I::PullAcc => {
                let acc = self.pop();
                self.cpu.acc_set(acc);
            }
            I::PullProcessorStatus => {
                let status = self.pop();
                self.cpu.status = status.into();
            }
            I::RotateLeft(b) => {
                let value = self.read(b);
                let rotated = (value << 1) | (self.cpu.status.carry as u8);
                self.cpu.status.carry = value.bit(7);
                self.write(b, rotated);
            }
            I::RotateRight(b) => {
                let value = self.read(b);
                let rotated = (value >> 1) | (self.cpu.status.carry as u8) << 7;
                self.cpu.status.carry = value.bit(0);
                self.write(b, rotated);
            }
            I::ReturnFromInterrupt => {
                let status = self.pop();
                self.cpu.status = status.into();
                let program_counter = self.pop_u16();
                self.cpu.pc = program_counter;
            }
            I::ReturnFromSubroutine => {
                let program_counter = self.pop_u16() + 1;
                self.cpu.pc = program_counter;
            }
            // TODO: Add tests for this. This might not be correct.
            I::SubtractWithCarry(b) => self
                .cpu
                .acc_add(!self.read(b) + self.cpu.status.carry as u8),
            I::SetCarry => self.cpu.status.carry = true,
            I::SetDecimal => self.cpu.status.decimal_mode = true,
            I::SetInterruptDisable => self.cpu.status.interrupt_disable = true,
            I::StoreAcc(b) => self.write(b, self.cpu.acc),
            I::StoreX(b) => self.write(b, self.cpu.x),
            I::StoreY(b) => self.write(b, self.cpu.y),
            I::TransferAccToX => self.cpu.x_set(self.cpu.acc),
            I::TransferAccToY => self.cpu.y_set(self.cpu.acc),
            I::TransferStackPointerToX => self.cpu.x_set(self.cpu.stack_pointer),
            I::TransferXToAcc => self.cpu.acc_set(self.cpu.x),
            I::TransferXToStackPointer => self.cpu.stack_pointer = self.cpu.x,
            I::TransferYToAcc => self.cpu.acc_set(self.cpu.y),
        }
    }

    fn push(&mut self, value: u8) {
        // 6502 uses an empty stack, meaning the stack pointer points to the element where the next value will be stored.
        if self.cpu.stack_pointer == 0 {
            eprintln!("WARNING: stack full, next push will overflow!");
        }
        self.memory[STACK_BASE as Address + self.cpu.stack_pointer as Address] = value;
        self.cpu.stack_pointer = self.cpu.stack_pointer.wrapping_sub(1);
    }

    fn push_u16(&mut self, value: u16) {
        let (high, low) = (value.bits(8..16) as u8, value.bits(0..8) as u8);
        self.push(high);
        self.push(low);
    }

    fn pop(&mut self) -> u8 {
        self.cpu.stack_pointer = self.cpu.stack_pointer.wrapping_add(1);
        self.memory[STACK_BASE as Address + self.cpu.stack_pointer as Address]
    }

    fn pop_u16(&mut self) -> u16 {
        let low = self.pop() as u16;
        let high = self.pop() as u16;
        (high << 8) | low
    }

    fn compare(&self, a: u8, b: u8) -> Status {
        Status {
            carry: a >= b,
            zero: a == b,
            negative: a.wrapping_sub(b).bit(7),
            ..self.cpu.status
        }
    }

    fn page_boundary_cycles(&self, location: ByteLocation) -> i128 {
        match location {
            ByteLocation::AbsoluteX(addr) => {
                ((addr + self.cpu.x as u16) % 0x100 < addr % 0x100) as i128
            }
            ByteLocation::AbsoluteY(addr) => {
                ((addr + self.cpu.y as u16) % 0x100 < addr % 0x100) as i128
            }
            ByteLocation::IndirectY(addr) => {
                ((addr as u16 + self.cpu.y as u16) % 0x100 < addr as u16 % 0x100) as i128
            }
            _ => 0,
        }
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
        self.memory[addr]
    }

    /// Modifies the byte located at `location`. Used in instructions which modify their arguments (e.g. ASL - Arithmetic Left Shift). Sets the zero and negative flags.
    fn write(&mut self, location: ByteLocation, value: u8) {
        use ByteLocation as B;
        self.cpu.status.zero = value == 0;
        self.cpu.status.negative = value.bit(7);
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
        self.memory[addr] = value;
    }

    fn get_le_u16(&self, addr: Address) -> u16 {
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

    fn x_set(&mut self, value: u8) {
        let zero = value == 0;
        let negative = value.bit(7);
        self.status = Status {
            zero,
            negative,
            ..self.status
        };
        self.x = value;
    }

    fn y_set(&mut self, value: u8) {
        let zero = value == 0;
        let negative = value.bit(7);
        self.status = Status {
            zero,
            negative,
            ..self.status
        };
        self.y = value;
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
            pc: 0xFFFC,
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
