use crate::nes::cpu::Address;

#[derive(Debug, Clone, Copy)]
enum Nametable {
    First,
    Second,
    Third,
    Fourth,
}

struct Registers {
    ctrl: Ctrl,
    mask: Mask,
    status: Status,
    oam_addr: u8,
    oam_data: u8,
    scroll: u8,
    addr: u8,
    data: u8,
    oam_dma: u8,
}

/// VPHB SINN
struct Ctrl {
    nametable_select: Nametable,
    increment_mode: bool,
    sprite_tile_select: bool,
    background_tile_select: bool,
    sprite_height: bool,
    ppu_master_slave_select: bool,
    nmi_enable: bool,
}

impl Nametable {
    fn addr(self) -> Address {
        0x2000 + 0x400 * self as u16
    }
}

/// BGRs bMmG
struct Mask {
    greyscale: bool,
    background_enable_left_column: bool,
    sprite_enable_left_column: bool,
    background_enable: bool,
    sprite_enable: bool,
    emphasize_red: bool,
    emphasize_green: bool,
    emphasize_blue: bool,
}

/// VSOx xxxx
struct Status {
    open_bus: u8,
    sprite_overflow: bool,
    sprite_0_hit: bool,
    vblank: bool,
}
