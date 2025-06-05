#[derive(Debug, Clone)]
pub enum Token {
    // One-character tokens
    Colon,
    SemiColon,
    Hashtag,
    Less,
    Greater,

    // Two-character tokens
    ColonEqual,
    NotEqual,
    EqualEqual,
    PlusEqual,
    MinusEqual,
    EqualMinus,
    OrEqual,
    AndEqual,
    XorEqual,
    ShiftRightEqual,
    ShiftLeftEqual,
    LessEqual,
    GreaterEqual,

    // Literals
    Identifier(String),
    Register(Register),
    Number(u16),

    // Keywords
    If,
    End,
    Loop,
    Jump,
    Jump0,
    Begin,
    Load,
    Save,
    Then,
    Return,
    Clear,
    Sprite,
    Bcd,

    // Directives
    Const,
    Alias,

    // Misc
    Eof,
    Error(TokenError),
    Comment(String),
}

#[derive(Debug, Clone)]
pub enum Register {
    Reg(u8),
    Delay,
    Buzzer,
    Index,
    Random,
    Key,
}

#[derive(Debug, Clone)]
pub enum TokenError {}

const token_literals: &[(Token, &str)] = &[
    (Token::Colon, ":"),
    (Token::SemiColon, ";"),
    (Token::Hashtag, "#"),
    (Token::Less, "<"),
    (Token::Greater, ">"),
    (Token::ColonEqual, ":="),
    (Token::NotEqual, "!="),
    (Token::EqualEqual, "=="),
    (Token::PlusEqual, "+="),
    (Token::MinusEqual, "-="),
    (Token::EqualMinus, "=-"),
    (Token::OrEqual, "|="),
    (Token::AndEqual, "&="),
    (Token::XorEqual, "^="),
    (Token::ShiftRightEqual, ">>="),
    (Token::ShiftLeftEqual, "<<="),
    (Token::LessEqual, "<="),
    (Token::GreaterEqual, ">="),
    (Token::If, "if"),
    (Token::End, "end"),
    (Token::Loop, "loop"),
    (Token::Jump, "jump"),
    (Token::Jump0, "jump0"),
    (Token::Begin, "begin"),
    (Token::Load, "load"),
    (Token::Save, "save"),
    (Token::Then, "then"),
    (Token::Return, "return"),
    (Token::Clear, "clear"),
    (Token::Sprite, "sprite"),
    (Token::Bcd, "bcd"),
    (Token::Const, ":const"),
    (Token::Alias, ":alias"),
];

// enum Expr {
//     UnaryExpr {
//         lhs: Box<Expr>,
//         op: UnaryOp,
//     },
//     BinaryExpr {
//         lhs: Box<Expr>,
//         rhs: Box<Expr>,
//         op: BinaryOp,
//     },
// }

// enum UnaryOp {
//     Neg,
// }

// enum BinaryOp {
//     Add,
//     Sub,
//     Mul,
//     Div,
//     And,
//     Or,
//     Xor,
// }
