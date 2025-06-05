mod token;

use core::panic;
use std::io::{self, Write};

use crate::chip8::instruction::Value;

use super::instruction::Instruction;

pub enum EncodedInstruction {
    Word(u16),
    Double(u16, u16),
}

pub fn compile<W>(instructions: &[Instruction], out: &mut W) -> Result<(), io::Error>
where
    W: Write,
{
    let opcodes: Vec<_> = instructions
        .iter()
        .flat_map(|instr| match encode(*instr) {
            EncodedInstruction::Word(op) => op.to_be_bytes().to_vec(),
            EncodedInstruction::Double(op1, op2) => {
                [op1.to_be_bytes(), op2.to_be_bytes()].concat().to_vec()
            }
        })
        .collect();
    out.write_all(opcodes.as_slice())?;
    Ok(())
}

pub fn encode(instr: Instruction) -> EncodedInstruction {
    fn ccnn(cc: u8, nn: u8) -> EncodedInstruction {
        EncodedInstruction::Word(((cc as u16) << 8) | nn as u16)
    }

    fn cnnn(c: u8, nnn: u16) -> EncodedInstruction {
        EncodedInstruction::Word(((c as u16) << 8) | nnn)
    }

    fn cxnn(c: u8, x: u8, nn: u8) -> EncodedInstruction {
        EncodedInstruction::Word(((c as u16) << 12) | ((x as u16) << 8) | nn as u16)
    }

    fn cxyn(c: u8, x: u8, y: u8, n: u8) -> EncodedInstruction {
        EncodedInstruction::Word(
            ((c as u16) << 12) | ((x as u16) << 8) | ((y as u16) << 4) | n as u16,
        )
    }

    use Instruction as I;

    match instr {
        I::Suspend => EncodedInstruction::Word(0x0000),
        I::ScrollDown(n) => EncodedInstruction::Word(0x00c0 | n as u16),
        I::ScrollUp(n) => EncodedInstruction::Word(0x00d0 | n as u16),
        I::Clear => EncodedInstruction::Word(0x00e0),
        I::Return => EncodedInstruction::Word(0x00ee),
        I::ScrollRight => EncodedInstruction::Word(0x00fb),
        I::ScrollLeft => EncodedInstruction::Word(0x00fc),
        I::ExitChip => EncodedInstruction::Word(0x00fd),
        I::DisableHighResolution => EncodedInstruction::Word(0x00fe),
        I::EnableHighResolution => EncodedInstruction::Word(0x00ff),
        I::Jump(nnn) => cnnn(0x1, nnn),
        I::Call(nnn) => cnnn(0x2, nnn),
        I::SkipIfEqual { x, v } => match v {
            Value::Immediate(nn) => cxnn(0x3, x, nn),
            Value::Register(y) => cxyn(0x5, x, y, 0x0),
        },
        I::SkipIfNotEqual { x, v } => match v {
            Value::Immediate(nn) => cxnn(0x4, x, nn),
            Value::Register(y) => cxyn(0x9, x, y, 0x0),
        },
        I::SaveRegisterRange { x, y } => cxyn(0x5, x, y, 0x2),
        I::LoadRegisterRange { x, y } => cxyn(0x5, x, y, 0x3),
        I::Load { x, v } => match v {
            Value::Immediate(nn) => cxnn(0x6, x, nn),
            Value::Register(y) => cxyn(0x8, x, y, 0x0),
        },
        I::Add { x, v } => match v {
            Value::Immediate(nn) => cxnn(0x7, x, nn),
            Value::Register(y) => cxyn(0x8, x, y, 0x4),
        },
        I::Or { x, y } => cxyn(0x8, x, y, 0x1),
        I::And { x, y } => cxyn(0x8, x, y, 0x2),
        I::Xor { x, y } => cxyn(0x8, x, y, 0x3),
        I::Sub { x, y } => cxyn(0x8, x, y, 0x5),
        I::ShiftRight { x, y } => cxyn(0x8, x, y, 0x6),
        I::SubNegative { x, y } => cxyn(0x8, x, y, 0x7),
        I::ShiftLeft { x, y } => cxyn(0x8, x, y, 0xe),
        I::LoadIndex(nnn) => cnnn(0xa, nnn),
        I::JumpOffset { x: _, addr } => cnnn(0xb, addr),
        I::Random { x, mask } => cxnn(0xc, x, mask),
        I::Draw { x, y, n } => cxyn(0xd, x, y, n),
        I::SkipIfKeyPressed { x } => cxnn(0xe, x, 0x9e),
        I::SkipIfKeyNotPressed { x } => cxnn(0xe, x, 0xa1),
        I::LoadLongIndex(addr) => match addr {
            Some(addr) => EncodedInstruction::Double(0xf000, addr),
            None => panic!("tried to encode LoadLongIndex with None as index"),
        },
        I::LoadAudio => EncodedInstruction::Word(0xf002),
        I::SelectPlane { mask } => cxnn(0xf, mask, 0x01),
        I::LoadDtIntoRegister { x } => cxnn(0xf, x, 0x07),
        I::LoadKeyPress { x } => cxnn(0xf, x, 0x0a),
        I::LoadRegisterIntoDt { x } => cxnn(0xf, x, 0x15),
        I::LoadRegisterIntoSt { x } => cxnn(0xf, x, 0x18),
        I::AddIndex { x } => cxnn(0xf, x, 0x1e),
        I::LoadFont { x } => cxnn(0xf, x, 0x29),
        I::LoadHiResFont { x } => cxnn(0xf, x, 0x30),
        I::LoadBcd { x } => cxnn(0xf, x, 0x33),
        I::SetPitch { x } => cxnn(0xf, x, 0x3a),
        I::StoreRegisters { x } => cxnn(0xf, x, 0x55),
        I::LoadRegisters { x } => cxnn(0xf, x, 0x65),
        I::StoreRegistersRPL { x } => cxnn(0xf, x, 0x75),
        I::LoadRegistersRPL { x } => cxnn(0xf, x, 0x85),
    }
}
