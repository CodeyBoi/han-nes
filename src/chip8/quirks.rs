/** Struct storing the differences in behaviour between the various Chip8 versions. */
#[derive(Debug)]
pub struct Quirks {
    /** VF Reset. The `AND`, `OR`, and `XOR` opcodes reset the flags register (VF) to zero. */
    pub vf_reset: bool,

    /** Memory. The register `SAVE` and `LOAD` opcodes increment the Index Register. */
    pub memory: bool,

    /** Display Wait. Drawing sprites waits for the vertical blank interrupt. */
    pub display_wait: bool,

    /** Clipping. Sprites drawn at the bottom edge of the screen get clipped instead of wrapping. */
    pub clipping: bool,

    /** Shifting. The `SHIFT` opcodes only operate on `Vx` instead of storing the shifted `Vy` in `Vx`. */
    pub shifting: bool,

    /** Jumping. The `JPV0` opcode doesn't use V0, but instead `Vx` where `x` is the highest 4 bits of `addr` (i.e. `xnn`). */
    pub jumping: bool,

    /** Scrolling. If the scrolling instructions operate on the "physical" or on the "logical" pixels in `lowres` mode */
    pub scrolling: ScrollingMode,
}

#[derive(Debug, Clone, Copy)]
pub enum ScrollingMode {
    Physical,
    Logical,
}

impl Quirks {
    pub const CHIP8: Self = Self {
        vf_reset: true,
        memory: true,
        display_wait: true,
        clipping: true,
        shifting: false,
        jumping: false,
        scrolling: ScrollingMode::Physical,
    };

    pub const SUPER_CHIP_LEGACY: Self = Self {
        vf_reset: false,
        memory: false,
        display_wait: true,
        clipping: true,
        shifting: true,
        jumping: true,
        scrolling: ScrollingMode::Physical,
    };

    pub const SUPER_CHIP_MODERN: Self = Self {
        vf_reset: false,
        memory: false,
        display_wait: false,
        clipping: true,
        shifting: true,
        jumping: true,
        scrolling: ScrollingMode::Logical,
    };

    pub const XO_CHIP: Self = Self {
        vf_reset: false,
        memory: true,
        display_wait: false,
        clipping: false,
        shifting: false,
        jumping: false,
        scrolling: ScrollingMode::Logical,
    };
}
