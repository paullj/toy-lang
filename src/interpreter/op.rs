use std::fmt::Display;

/// Represents an operation for the virtual machine
/// We have limited operations, so we can use a u8 to represent them
#[repr(u8)]
pub enum Op {
    /// Reads a 1-byte index from the constants table and pushes it onto the stack
    Constant = 0,
    /// Returns the top value from the stack
    Return,
    /// Pushes a true value onto the stack
    True,
    /// Pushes a false value onto the stack
    False,
    /// Pops two values from the stack and compares them for equality, pushing the result onto the stack
    Equal,
    /// Pops two values from the stack and compares if the first is less than the second, pushing the result onto the stack
    Less,
    /// Pops two values from the stack and compares if the first is less than or equal to the second, pushing the result onto the stack
    LessEqual,
    /// Pops two values from the stack and compares if the first is greater than the second, pushing the result onto the stack
    Greater,
    /// Pops two values from the stack and compares if the first is greater than or equal to the second, pushing the result onto the stack
    GreaterEqual,
    /// Pops a value from the stack and checks if it is false, pushing the result onto the stack
    Not,
    /// Pops a value from the stack and negates it, pushing the result onto the stack
    Negate,
    /// Pops two values from the stack and adds them, pushing the result onto the stack
    Add,
    /// Pops two values from the stack and subtracts them, pushing the result onto the stack
    Subtract,
    /// Pops two values from the stack and multiplies them, pushing the result onto the stack
    Multiply,
    /// Pops two values from the stack and divides them, pushing the result onto the stack
    Divide,
    /// Pops a value from the stack and prints it
    Echo,
    /// Pops a value from the stack
    Pop,
    /// Pops a number of values from the stack
    PopN,
    /// Defines a global variable
    DefineGlobal,
    /// Gets a global variable
    GetGlobal,
    /// Sets a global variable
    SetGlobal,
    /// Gets a local variable
    GetLocal,
    /// Sets a local variable
    SetLocal,
    /// Jumps to an instruction if the top value from the stack is false
    JumpIfFalse,
    /// Jumps to an instruction
    Jump,
    /// Loops to an instruction
    Loop,
    /// Call a function
    Call,
}

impl From<Op> for u8 {
    fn from(op: Op) -> Self {
        op as u8
    }
}

impl From<u8> for Op {
    fn from(op: u8) -> Self {
        match op {
            0 => Op::Constant,
            1 => Op::Return,
            2 => Op::True,
            3 => Op::False,
            4 => Op::Equal,
            5 => Op::Less,
            6 => Op::LessEqual,
            7 => Op::Greater,
            8 => Op::GreaterEqual,
            9 => Op::Not,
            10 => Op::Negate,
            11 => Op::Add,
            12 => Op::Subtract,
            13 => Op::Multiply,
            14 => Op::Divide,
            15 => Op::Echo,
            16 => Op::Pop,
            17 => Op::PopN,
            18 => Op::DefineGlobal,
            19 => Op::GetGlobal,
            20 => Op::SetGlobal,
            21 => Op::GetLocal,
            22 => Op::SetLocal,
            23 => Op::JumpIfFalse,
            24 => Op::Jump,
            25 => Op::Loop,
            26 => Op::Call,
            _ => panic!("Unknown opcode: {}", op),
        }
    }
}

impl Op {
    pub fn size(&self) -> usize {
        match self {
            Op::Constant => 2,
            Op::Return => 1,
            Op::True => 1,
            Op::False => 1,
            Op::Not => 1,
            Op::Negate => 1,
            Op::Add => 1,
            Op::Subtract => 1,
            Op::Multiply => 1,
            Op::Divide => 1,
            Op::Equal => 1,
            Op::Greater => 1,
            Op::Less => 1,
            Op::LessEqual => 1,
            Op::GreaterEqual => 1,
            Op::Echo => 1,
            Op::Pop => 1,
            Op::PopN => 2,
            Op::DefineGlobal => 2,
            Op::GetGlobal => 2,
            Op::SetGlobal => 2,
            Op::GetLocal => 2,
            Op::SetLocal => 2,
            Op::JumpIfFalse => 3,
            Op::Jump => 3,
            Op::Loop => 3,
            Op::Call => 2,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Return => write!(f, "RETURN"),
            Op::Constant => write!(f, "CONSTANT"),
            Op::True => write!(f, "TRUE"),
            Op::False => write!(f, "FALSE"),
            Op::Not => write!(f, "NOT"),
            Op::Negate => write!(f, "NEGATE"),
            Op::Add => write!(f, "ADD"),
            Op::Subtract => write!(f, "SUBTRACT"),
            Op::Multiply => write!(f, "MULTIPLY"),
            Op::Divide => write!(f, "DIVIDE"),
            Op::Equal => write!(f, "EQUAL"),
            Op::Less => write!(f, "LESS"),
            Op::Greater => write!(f, "GREATER"),
            Op::LessEqual => write!(f, "LESS_EQUAL"),
            Op::GreaterEqual => write!(f, "GREATER_EQUAL"),
            Op::Echo => write!(f, "ECHO"),
            Op::Pop => write!(f, "POP"),
            Op::DefineGlobal => write!(f, "DEFINE_GLOBAL"),
            Op::GetGlobal => write!(f, "GET_GLOBAL"),
            Op::SetGlobal => write!(f, "SET_GLOBAL"),
            Op::GetLocal => write!(f, "GET_LOCAL"),
            Op::SetLocal => write!(f, "SET_LOCAL"),
            Op::PopN => write!(f, "POP_N"),
            Op::JumpIfFalse => write!(f, "JUMP_IF_FALSE"),
            Op::Jump => write!(f, "JUMP"),
            Op::Loop => write!(f, "LOOP"),
            Op::Call => write!(f, "CALL"),
        }
    }
}
