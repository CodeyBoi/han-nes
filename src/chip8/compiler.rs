use crate::chip8::instruction::{self, Value};

use super::instruction::Instruction;

pub fn encode(instr: Instruction) -> u16 {
    use Instruction as I;

    fn ccnn(cc: u8, nn: u8) -> u16 {
        ((cc as u16) << 8) | nn as u16
    }

    fn cnnn(c: u8, nnn: u16) -> u16 {
        ((c as u16) << 8) | nnn
    }

    fn cxnn(c: u8, x: u8, nn: u8) -> u16 {
        ((c as u16) << 12) | ((x as u16) << 8) | nn as u16
    }

    fn cxyn(c: u8, x: u8, y: u8, n: u8) -> u16 {
        ((c as u16) << 12) | ((x as u16) << 8) | ((y as u16) << 4) | n as u16
    }

    match instr {
        I::Suspend => 0x0000,
        I::ScrollDown(n) => 0x00c0 | n as u16,
        I::ScrollUp(n) => 0x00d0 | n as u16,
        I::Clear => 0x00e0,
        I::Return => 0x00ee,
        I::ScrollRight => 0x00fb,
        I::ScrollLeft => 0x00fc,
        I::ExitChip => 0x00fd,
        I::DisableExtendedScreen => 0x00fe,
        I::EnableExtendedScreen => 0x00ff,
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
        I::Random { x, mask } => todo!(),
        I::Draw { x, y, n } => todo!(),
        I::SkipIfKeyPressed { x } => todo!(),
        I::SkipIfKeyNotPressed { x } => todo!(),
        I::LoadLongIndex => todo!(),
        I::LoadAudio => todo!(),
        I::SelectPlane { mask } => todo!(),
        I::LoadDtIntoRegister { x } => todo!(),
        I::LoadKeyPress { x } => todo!(),
        I::LoadRegisterIntoDt { x } => todo!(),
        I::LoadRegisterIntoSt { x } => todo!(),
        I::AddIndex { x } => todo!(),
        I::LoadFont { x } => todo!(),
        I::LoadHiResFont { x } => todo!(),
        I::LoadBcd { x } => todo!(),
        I::SetPitch { x } => todo!(),
        I::StoreRegisters { x } => todo!(),
        I::LoadRegisters { x } => todo!(),
        I::StoreRegistersRPL { x } => todo!(),
        I::LoadRegistersRPL { x } => todo!(),
    }
}
