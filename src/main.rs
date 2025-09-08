use std::path::PathBuf;

use chip8::Chip8;
use clap::{CommandFactory, Parser, Subcommand};

mod bits;
mod chip8;
mod nes;

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// A Chip8 emulator, capable of running `ch8` ROMs
    Chip8(Chip8Args),
}

#[derive(Parser, Debug)]
#[command(version, about)]
struct Chip8Args {
    /// Which dir to search for ROMs to present in the ROM Selector TUI
    #[arg(short = 'd', long, default_value = "chip8-roms")]
    roms_dir: PathBuf,

    /// The ROM to load and run on startup
    #[arg(short, long)]
    binary: Option<PathBuf>,

    /// Which chip type to emulate
    #[arg(short = 't', long, value_enum, default_value_t = chip8::ChipType::Chip8)]
    chip_type: chip8::ChipType,
}

fn main() {
    let (mut cmd, args) = (Cli::command(), Cli::parse());
    match args.command {
        Some(Command::Chip8(args)) => {
            let mut chip = Chip8::new(args.chip_type);
            let rom = match args.binary {
                Some(rom) => rom,
                None => chip8::run_rom_selector_cli(&args.roms_dir),
            };
            chip.load_rom(&rom).expect("ROM too large for memory");
            chip.run();
        }
        None => cmd.print_help().expect("failed printing error message"),
    }
}
