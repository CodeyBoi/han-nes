use std::ops::{Index, IndexMut};

use super::cpu::Address;

pub const RAM_BASE: usize = 0x0000;
pub const RAM_SIZE: usize = 0x800;
pub const RAM_END: usize = RAM_BASE + RAM_SIZE;
pub const STACK_BASE: usize = 0x100;
pub const STACK_SIZE: usize = 0x100;
pub const PPU_BASE: usize = 0x2000;
pub const IO_BASE: usize = 0x4000;
pub const ROM_BASE: usize = 0x8000;
pub const ROM_SIZE: usize = 0x8000;
pub const ROM_END: usize = ROM_BASE + ROM_SIZE;

pub struct MemoryMap {
    ram: [u8; RAM_SIZE],
    program_rom: [u8; ROM_SIZE],
}

impl MemoryMap {
    pub fn new() -> Self {
        Self {
            ram: [0x0; RAM_SIZE],
            program_rom: [0x0; ROM_SIZE],
        }
    }

    pub fn load_program_rom(&mut self, data: &[u8]) {
        self.program_rom[..data.len()].copy_from_slice(data);
        if data.len() < self.program_rom.len() {
            self.program_rom[data.len()..].copy_from_slice(data);
        }
    }

    pub fn slice_from(&self, addr: Address) -> &[u8] {
        let addr = addr as usize;
        match addr {
            RAM_BASE..RAM_END => &self.ram[addr - RAM_BASE..RAM_SIZE],
            ROM_BASE..ROM_END => &self.program_rom[addr - ROM_BASE..ROM_SIZE],
            _ => panic!("invalid memory access @ {:X}", addr),
        }
    }
}

impl Index<Address> for MemoryMap {
    type Output = u8;

    fn index(&self, index: Address) -> &Self::Output {
        let addr = index as usize;
        match addr {
            RAM_BASE..RAM_END => &self.ram[addr - RAM_BASE],
            ROM_BASE..ROM_END => &self.program_rom[addr - ROM_BASE],
            _ => panic!("invalid memory access @ {:X}", addr),
        }
    }
}

impl IndexMut<Address> for MemoryMap {
    fn index_mut(&mut self, index: Address) -> &mut Self::Output {
        let addr = index as usize;
        match addr {
            RAM_BASE..RAM_END => &mut self.ram[addr - RAM_BASE],
            ROM_BASE..ROM_END => &mut self.program_rom[addr - ROM_BASE],
            _ => panic!("invalid memory access @ {:X}", addr),
        }
    }
}
