use std::borrow::Cow;

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

pub(crate) struct Compiler<'a> {
    parser: Parser<'a>,
    chunk: Chunk,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

struct Local<'a> {
    name: Cow<'a, str>,
    depth: Option<usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chunk: Chunk::new(),
            parser: Parser::new(source),
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub(crate) fn compile(mut self) -> Result<Chunk, String> {
        let ast = self.parser.parse().map_err(|e| e.to_string())?;

        self.compile_tree(&ast)?;
        self.emit_op(Op::Return, &(0..0).into());
        Ok(self.chunk)
    }

    fn compile_tree(&mut self, tree: &Tree<'a>) -> Result<(), String> {
        match tree {
            Tree::Atom(atom, span) => self.compile_atom(atom, span),
            Tree::Construct(op, args, span) => self.compile_cons(op, args, span),
        }
    }

    fn compile_atom(&mut self, atom: &Atom, span: &Span) -> Result<(), String> {
        match atom {
            Atom::Float(n) => {
                if let Some(constant) = self.chunk.write_constant(Value::Float(*n)) {
                    self.emit_op_with_arg(Op::Constant, constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Int(n) => {
                if let Some(constant) = self.chunk.write_constant(Value::Int(*n)) {
                    self.emit_op_with_arg(Op::Constant, constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Bool(b) => {
                if let Some(constant) = self.chunk.write_constant(Value::Bool(*b)) {
                    self.emit_op_with_arg(Op::Constant, constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::String(s) => {
                if let Some(constant) = self.chunk.write_constant(Value::String(s.to_string())) {
                    self.emit_op_with_arg(Op::Constant, constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Identifier(s) => {
                let (arg, get_op, _) = match self.get_variable(s) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.emit_op_with_arg(get_op, arg, span);
            }
        }
        Ok(())
    }

    fn compile_cons(
        &mut self,
        op: &Operator,
        args: &[Tree<'a>],
        span: &Span,
    ) -> Result<(), String> {
        match (op, args) {
            (Operator::Root, args) => {
                for value in args {
                    self.compile_tree(&value)?;
                }
            }
            (Operator::Minus, [value]) => {
                self.compile_tree(&value)?;
                self.emit_op(Op::Negate, span);
            }
            (Operator::Bang, [value]) => {
                self.compile_tree(&value)?;
                self.emit_op(Op::Not, span);
            }
            (
                Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Slash,
                [left, right],
            ) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                match op {
                    Operator::Plus => self.emit_op(Op::Add, span),
                    Operator::Minus => self.emit_op(Op::Subtract, span),
                    Operator::Asterisk => self.emit_op(Op::Multiply, span),
                    Operator::Slash => self.emit_op(Op::Divide, span),
                    _ => unreachable!(),
                };
            }
            (Operator::Group, args) => {
                self.begin_scope();
                for value in args {
                    self.compile_tree(&value)?;
                }
                self.end_scope();
            }
            (Operator::EqualEqual, [left, right]) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                self.emit_op(Op::Equal, span);
            }
            (Operator::Less, [left, right]) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                self.emit_op(Op::Less, span);
            }
            (Operator::Greater, [left, right]) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                self.emit_op(Op::Greater, span);
            }
            (Operator::LessEqual, [left, right]) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                self.emit_op(Op::LessEqual, span);
            }
            (Operator::GreaterEqual, [left, right]) => {
                self.compile_tree(&left)?;
                self.compile_tree(&right)?;
                self.emit_op(Op::GreaterEqual, span);
            }
            (Operator::Echo, [value]) => {
                self.compile_tree(&value)?;
                self.emit_op(Op::Echo, span);
            }
            (Operator::Return, _) => {
                self.emit_op(Op::Return, span);
            }
            (Operator::Let, [Tree::Atom(Atom::Identifier(name), _), value]) => {
                if self.scope_depth != 0 {
                    if self.locals.iter().filter(|x| x.name == *name).count() != 0 {
                        return Err("Variable already declared with the same name".to_string());
                    } else {
                        let loc = Local {
                            name: name.clone(),
                            depth: None,
                        };
                        self.locals.push(loc);
                        let last = self.locals.len() - 1;
                        self.locals[last].depth = Some(self.scope_depth);
                    }
                }

                if let Some(constant) = self.chunk.write_constant(Value::String(name.to_string())) {
                    self.compile_tree(&value)?;
                    if self.scope_depth == 0 {
                        self.emit_op_with_arg(Op::DefineGlobal, constant, span);
                    }
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            (Operator::Equal, [Tree::Atom(Atom::Identifier(name), _), value]) => {
                let (arg, _, set_op) = match self.get_variable(name) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.compile_tree(&value)?;
                self.emit_op_with_arg(set_op, arg, span);
            }
            (Operator::And, [left, right]) => {
                self.compile_tree(&left)?;
                let offset_jump = self.emit_op_with_args(Op::JumpIfFalse, [0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(right)?;
                self.patch_jump(offset_jump);
            }
            (Operator::Or, [left, right]) => {
                self.compile_tree(&left)?;
                let else_jump = self.emit_op_with_args(Op::JumpIfFalse, [0xff, 0xff], span);
                let end_jump = self.emit_op_with_args(Op::Jump, [0xff, 0xff], span);

                self.patch_jump(else_jump);
                self.emit_op(Op::Pop, span);

                self.compile_tree(&right)?;
                self.patch_jump(end_jump);
            }
            (Operator::If, [condition, then_branch]) => {
                self.compile_tree(&condition)?;
                let then_offset = self.emit_op_with_args(Op::JumpIfFalse, [0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(&then_branch)?;
                self.patch_jump(then_offset);
            }
            (Operator::If, [condition, then_branch, else_branch]) => {
                self.compile_tree(&condition)?;
                let then_offset = self.emit_op_with_args(Op::JumpIfFalse, [0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(&then_branch)?;

                let else_offset = self.emit_op_with_args(Op::Jump, [0xff, 0xff], span);
                self.patch_jump(then_offset);
                self.emit_op(Op::Pop, span);

                self.compile_tree(&else_branch)?;
                self.patch_jump(else_offset);
            }
            (op, _) => todo!("Implement operator in compiler.rs {op:?}"),
        }
        Ok(())
    }

    /// Emits an operation to the chunk and returns the index of the operation
    fn emit_op(&mut self, op: Op, span: &Span) -> usize {
        self.chunk.write_u8(op.into(), span);
        self.chunk.count()
    }

    /// Emits an operation with a single argument to the chunk and returns the index of the operation
    fn emit_op_with_arg(&mut self, op: Op, arg: u8, span: &Span) -> usize {
        self.emit_op(op, span);
        self.chunk.write_u8(arg, span);
        self.chunk.count() - 1
    }

    /// Emits an operation with two arguments to the chunk and returns the index of the operation
    fn emit_op_with_args(&mut self, op: Op, args: [u8; 2], span: &Span) -> usize {
        self.emit_op(op, span);
        self.chunk.write_u8(args[0], span);
        self.chunk.write_u8(args[1], span);
        self.chunk.count() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let relative = self.chunk.count() - offset - 2;
        if relative > u16::MAX as usize {
            panic!("Too much code to jump over.");
        }
        self.chunk.update_u8(offset, ((relative >> 8) & 0xff) as u8);
        self.chunk.update_u8(offset + 1, (relative & 0xff) as u8);
    }

    fn get_variable(&mut self, name: &Cow<str>) -> Result<(u8, Op, Op), ()> {
        if let Some(local_arg) = self.resolve_local(name) {
            return Ok((local_arg, Op::GetLocal, Op::SetLocal));
        } else if let Some(global_arg) = self.chunk.write_constant(Value::String(name.to_string()))
        {
            return Ok((global_arg, Op::GetGlobal, Op::SetGlobal));
        }
        Err(())
    }

    fn resolve_local(&self, name: &Cow<str>) -> Option<u8> {
        for (e, v) in self.locals.iter().rev().enumerate() {
            if v.name == *name {
                if v.depth.is_none() {
                    todo!("Can't read local variable in its own initializer.");
                }
                return Some((self.locals.len() - e - 1) as u8);
            }
        }
        None
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        let mut keep = self.locals.len();
        while keep > 0 && self.locals[keep - 1].depth.unwrap() > self.scope_depth {
            keep -= 1;
        }
        let pops = self.locals.len() - keep;
        if pops > 0 {
            self.emit_op_with_arg(Op::PopN, pops as u8, &(0..0).into());
            self.locals.truncate(keep);
        }
    }
}
