use std::ops::{Index, IndexMut};

use super::cpu::Address;

pub const RAM_BASE: usize = 0x0000;
pub const RAM_SIZE: usize = 0x800;
pub const RAM_END: usize = RAM_BASE + RAM_SIZE;
pub const RAM_MIRRORS: usize = 4;

pub const STACK_BASE: usize = 0x100;
pub const STACK_SIZE: usize = 0x100;

pub const PPU_BASE: usize = 0x2000;
pub const PPU_SIZE: usize = 0x8;
pub const PPU_END: usize = PPU_BASE + PPU_SIZE;
pub const PPU_MIRRORS: usize = 0x2000 / PPU_SIZE;

pub const IO_BASE: usize = 0x4000;
pub const IO_SIZE: usize = 0x18;
pub const IO_END: usize = IO_BASE + IO_SIZE;

pub const ROM_BASE: usize = 0x8000;
pub const ROM_SIZE: usize = 0x8000;
pub const ROM_END: usize = ROM_BASE + ROM_SIZE;

pub struct MemoryMap {
    ram: [u8; RAM_SIZE],
    ppu: [u8; PPU_SIZE],
    io: [u8; IO_SIZE],
    program_rom: [u8; ROM_SIZE],
}

impl MemoryMap {
    pub fn new() -> Self {
        Self {
            ram: [0x0; RAM_SIZE],
            ppu: [0x0; PPU_SIZE],
            io: [0x0; IO_SIZE],
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
        const RAM_MIRRORS_END: usize = RAM_BASE + RAM_SIZE * RAM_MIRRORS;
        const PPU_MIRRORS_END: usize = PPU_BASE + PPU_SIZE * PPU_MIRRORS;
        match addr {
            RAM_BASE..RAM_MIRRORS_END => &self.ram[(addr - RAM_BASE) % RAM_SIZE..],
            PPU_BASE..PPU_MIRRORS_END => &self.ppu[(addr - PPU_BASE) % PPU_SIZE..],
            IO_BASE..IO_END => &self.io[addr - IO_BASE..],
            ROM_BASE..ROM_END => &self.program_rom[addr - ROM_BASE..],
            _ => panic!("invalid memory access @ ${:X}", addr),
        }
    }
}

impl Index<Address> for MemoryMap {
    type Output = u8;

    fn index(&self, index: Address) -> &Self::Output {
        let addr = index as usize;
        const RAM_MIRRORS_END: usize = RAM_BASE + RAM_SIZE * RAM_MIRRORS;
        const PPU_MIRRORS_END: usize = PPU_BASE + PPU_SIZE * PPU_MIRRORS;
        match addr {
            RAM_BASE..RAM_MIRRORS_END => &self.ram[(addr % RAM_SIZE) - RAM_BASE],
            PPU_BASE..PPU_MIRRORS_END => &self.ppu[(addr - PPU_BASE) % PPU_SIZE],
            IO_BASE..IO_END => &self.io[addr - IO_BASE],
            ROM_BASE..ROM_END => &self.program_rom[addr - ROM_BASE],
            _ => panic!("invalid memory read access @ ${:X}", addr),
        }
    }
}

impl IndexMut<Address> for MemoryMap {
    fn index_mut(&mut self, index: Address) -> &mut Self::Output {
        let addr = index as usize;
        const RAM_MIRRORS_END: usize = RAM_BASE + RAM_SIZE * RAM_MIRRORS;
        const PPU_MIRRORS_END: usize = PPU_BASE + PPU_SIZE * PPU_MIRRORS;
        match addr {
            RAM_BASE..RAM_MIRRORS_END => &mut self.ram[(addr % RAM_SIZE) - RAM_BASE],
            PPU_BASE..PPU_MIRRORS_END => &mut self.ppu[(addr - PPU_BASE) % PPU_SIZE],
            IO_BASE..IO_END => &mut self.io[addr - IO_BASE],
            ROM_BASE..ROM_END => &mut self.program_rom[addr - ROM_BASE],
            _ => panic!("invalid memory write access @ ${:X}", addr),
        }
    }
}
