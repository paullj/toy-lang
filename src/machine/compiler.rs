use crate::{
    error::Span,
    syntax::{
        ast::{Atom, Operator, Tree},
        parse::Parser,
    },
};

use super::chunk::{Chunk, Op};

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
            if let Some(constant) = chunk.write_constant(*n) {
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
    match op {
        Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Slash => {
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
        Operator::Group => {
            // Just compile the inner expression
            compile_tree(&args[0], chunk, span)?;
        }
        _ => todo!("Implement other operators"),
    }
    Ok(())
}
