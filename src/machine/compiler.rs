use crate::{
    error::Span,
    syntax::{
        ast::{Atom, Operator, Tree},
        parse::Parser,
    },
};

use super::{
    chunk::{Chunk, Op},
    value::Value,
};

pub(crate) fn compile(source: &str) -> Result<Chunk, String> {
    let mut parser = Parser::new(source);
    let ast = parser.parse().map_err(|e| e.to_string())?;
    let mut chunk = Chunk::new();
    let span = Span { start: 0, end: 0 }; // TODO: Track spans properly

    compile_tree(&ast, &mut chunk, &span)?;
    chunk.write_u8(Op::Return.into(), &span);

    Ok(chunk)
}

fn compile_tree<'a>(tree: &Tree<'a>, chunk: &mut Chunk, span: &Span) -> Result<(), String> {
    match tree {
        Tree::Atom(atom) => compile_atom(atom, chunk, span),
        Tree::Cons(op, args) => compile_cons(op, args, chunk, span),
        _ => todo!("Implement other tree types"),
    }
}

fn compile_atom<'a>(atom: &Atom<'a>, chunk: &mut Chunk, span: &Span) -> Result<(), String> {
    match atom {
        Atom::Float(n) => {
            if let Some(constant) = chunk.write_constant(Value::Float(*n)) {
                chunk.write_u8(Op::Constant.into(), span);
                chunk.write_u8(constant, span);
            } else {
                return Err("Too many constants".to_string());
            }
        }
        Atom::Int(n) => {
            if let Some(constant) = chunk.write_constant(Value::Int(*n)) {
                chunk.write_u8(Op::Constant.into(), span);
                chunk.write_u8(constant, span);
            } else {
                return Err("Too many constants".to_string());
            }
        }
        Atom::Bool(b) => {
            if let Some(constant) = chunk.write_constant(Value::Bool(*b)) {
                chunk.write_u8(Op::Constant.into(), span);
                chunk.write_u8(constant, span);
            } else {
                return Err("Too many constants".to_string());
            }
        }
        Atom::String(s) => {
            if let Some(constant) = chunk.write_constant(Value::String(s.to_string())) {
                chunk.write_u8(Op::Constant.into(), span);
                chunk.write_u8(constant, span);
            } else {
                return Err("Too many constants".to_string());
            }
        }
        _ => todo!("Implement other atom types"),
    }
    Ok(())
}

fn compile_cons<'a>(
    op: &Operator,
    args: &[Tree<'a>],
    chunk: &mut Chunk,
    span: &Span,
) -> Result<(), String> {
    match (op, args.len()) {
        (Operator::Minus, 1) => {
            // Compile the inner expression
            compile_tree(&args[0], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Negate.into(), span);
        }
        (Operator::Bang, 1) => {
            // Compile the inner expression
            compile_tree(&args[0], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Not.into(), span);
        }
        (Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Slash, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;

            // Compile right operand
            compile_tree(&args[1], chunk, span)?;

            // Write the operation
            match op {
                Operator::Plus => chunk.write_u8(Op::Add.into(), span),
                Operator::Minus => chunk.write_u8(Op::Subtract.into(), span),
                Operator::Asterisk => chunk.write_u8(Op::Multiply.into(), span),
                Operator::Slash => chunk.write_u8(Op::Divide.into(), span),
                _ => unreachable!(),
            }
        }
        (Operator::Group, _) => {
            // Just compile the inner expression
            compile_tree(&args[0], chunk, span)?;
        }
        (Operator::EqualEqual, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;
            // Compile right operand
            compile_tree(&args[1], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Equal.into(), span);
        }
        (Operator::Less, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;
            // Compile right operand
            compile_tree(&args[1], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Less.into(), span);
        }
        (Operator::Greater, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;
            // Compile right operand
            compile_tree(&args[1], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Greater.into(), span);
        }
        (Operator::LessEqual, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;
            // Compile right operand
            compile_tree(&args[1], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::LessEqual.into(), span);
        }
        (Operator::GreaterEqual, 2) => {
            // Compile left operand
            compile_tree(&args[0], chunk, span)?;
            // Compile right operand
            compile_tree(&args[1], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::GreaterEqual.into(), span);
        }
        (Operator::Echo, 1) => {
            // Compile the inner expression
            compile_tree(&args[0], chunk, span)?;
            // Write the operation
            chunk.write_u8(Op::Echo.into(), span);
        }
        (Operator::Return, _) => {
            // Write the operation
            chunk.write_u8(Op::Return.into(), span);
        }
        (op, _) => todo!("Implement other operators {op:?}"),
    }
    Ok(())
}
