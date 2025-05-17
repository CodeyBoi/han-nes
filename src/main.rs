use chip8::Chip8;

mod chip8;

fn main() {
    let mut c8 = Chip8::new();
    c8.load_rom("chip8-roms/games/Pong (1 player).ch8").unwrap();
    c8.run();
}
