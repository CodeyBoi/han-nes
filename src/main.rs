use std::{
    io::{self, BufRead, Write},
    path::Path,
};

use chip8::Chip8;
use clap::Parser;

mod chip8;

#[derive(clap::Parser)]
#[command(version, about)]
struct Cli {
    /// Which dir to search for ROMs
    #[arg(short = 'd', long, default_value = "chip8-roms")]
    rom_dir: String,
}

fn main() {
    let args = Cli::parse();

    let stdin = io::stdin();
    let mut rom_paths = chip8::find_roms(Path::new(&args.rom_dir), true);

    rom_paths.sort_by(|a, b| a.file_name().cmp(&b.file_name()));

    println!(
        "{}",
        rom_paths
            .iter()
            .enumerate()
            .map(|(i, p)| format!(
                "{: >3}. {}\n",
                i + 1,
                p.file_name().unwrap().to_str().unwrap()
            ))
            .fold(String::new(), |mut acc, p| {
                acc.push_str(&p);
                acc
            })
    );

    let choice = loop {
        print!("Choose a ROM: ");
        io::stdout().flush().unwrap();
        let input = stdin.lock().lines().next().unwrap().unwrap();
        if let Ok(choice) = input.trim().parse::<usize>() {
            if choice - 1 < rom_paths.len() {
                break choice - 1;
            } else {
                println!("Index {} out of range.", choice);
            }
        } else {
            println!("Error parsing {}", input);
        }
    };

    println!(
        "Running {}...",
        rom_paths[choice].file_name().unwrap().to_str().unwrap()
    );

    let mut c8 = Chip8::new();
    c8.load_rom(&rom_paths[choice]).unwrap();
    c8.run();
}
