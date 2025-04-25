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
        self.chunk.write_u8(Op::Return.into(), &(0..0).into());
        Ok(self.chunk)
    }

    fn compile_tree(&mut self, tree: &Tree<'a>) -> Result<(), String> {
        match tree {
            Tree::Atom(atom, span) => self.compile_atom(atom, span),
            Tree::Cons(op, args, span) => self.compile_cons(op, args, span),
            _ => todo!("Implement other tree types"),
        }
    }

    fn compile_atom(&mut self, atom: &Atom, span: &Span) -> Result<(), String> {
        match atom {
            Atom::Float(n) => {
                if let Some(constant) = self.chunk.write_constant(Value::Float(*n)) {
                    self.chunk.write_u8(Op::Constant.into(), span);
                    self.chunk.write_u8(constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Int(n) => {
                if let Some(constant) = self.chunk.write_constant(Value::Int(*n)) {
                    self.chunk.write_u8(Op::Constant.into(), span);
                    self.chunk.write_u8(constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Bool(b) => {
                if let Some(constant) = self.chunk.write_constant(Value::Bool(*b)) {
                    self.chunk.write_u8(Op::Constant.into(), span);
                    self.chunk.write_u8(constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::String(s) => {
                if let Some(constant) = self.chunk.write_constant(Value::String(s.to_string())) {
                    self.chunk.write_u8(Op::Constant.into(), span);
                    self.chunk.write_u8(constant, span);
                } else {
                    return Err("Too many constants".to_string());
                }
            }
            Atom::Identifier(s) => {
                let (arg, get_op, _) = match self.get_variable(s) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.chunk.write_u8(get_op.into(), span);
                self.chunk.write_u8(arg, span);
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
        // TODO: Make this better. Surely can't be the best way to do this.
        match (op, args) {
            (Operator::Minus, [value]) => {
                // Compile the inner expression
                self.compile_tree(&value)?;
                // Write the operation
                self.chunk.write_u8(Op::Negate.into(), span);
            }
            (Operator::Bang, [value]) => {
                // Compile the inner expression
                self.compile_tree(&value)?;
                // Write the operation
                self.chunk.write_u8(Op::Not.into(), span);
            }
            (
                Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Slash,
                [left, right],
            ) => {
                // Compile left operand
                self.compile_tree(&left)?;

                // Compile right operand
                self.compile_tree(&right)?;

                // Write the operation
                match op {
                    Operator::Plus => self.chunk.write_u8(Op::Add.into(), span),
                    Operator::Minus => self.chunk.write_u8(Op::Subtract.into(), span),
                    Operator::Asterisk => self.chunk.write_u8(Op::Multiply.into(), span),
                    Operator::Slash => self.chunk.write_u8(Op::Divide.into(), span),
                    _ => unreachable!(),
                }
            }
            (Operator::Root, args) => {
                for value in args {
                    self.compile_tree(&value)?;
                }
            }
            (Operator::Group, args) => {
                self.begin_scope();
                for value in args {
                    self.compile_tree(&value)?;
                }
                self.end_scope();
            }
            (Operator::EqualEqual, [left, right]) => {
                // Compile left operand
                self.compile_tree(&left)?;
                // Compile right operand
                self.compile_tree(&right)?;
                // Write the operation
                self.chunk.write_u8(Op::Equal.into(), span);
            }
            (Operator::Less, [left, right]) => {
                // Compile left operand
                self.compile_tree(&left)?;
                // Compile right operand
                self.compile_tree(&right)?;
                // Write the operation
                self.chunk.write_u8(Op::Less.into(), span);
            }
            (Operator::Greater, [left, right]) => {
                // Compile left operand
                self.compile_tree(&left)?;
                // Compile right operand
                self.compile_tree(&right)?;
                // Write the operation
                self.chunk.write_u8(Op::Greater.into(), span);
            }
            (Operator::LessEqual, [left, right]) => {
                // Compile left operand
                self.compile_tree(&left)?;
                // Compile right operand
                self.compile_tree(&right)?;
                // Write the operationj
                self.chunk.write_u8(Op::LessEqual.into(), span);
            }
            (Operator::GreaterEqual, [left, right]) => {
                // Compile left operand
                self.compile_tree(&left)?;
                // Compile right operand
                self.compile_tree(&right)?;
                // Write the operation
                self.chunk.write_u8(Op::GreaterEqual.into(), span);
            }
            (Operator::Echo, [value]) => {
                // Compile the inner expression
                self.compile_tree(&value)?;
                // Write the operation
                self.chunk.write_u8(Op::Echo.into(), span);
            }
            (Operator::Return, _) => {
                // Write the operation
                self.chunk.write_u8(Op::Return.into(), span);
            }
            (Operator::Let, [Tree::Atom(Atom::Identifier(name), _), value]) => {
                // // self.parse_variable("Expect variable name.");
                // // self.declare_variable();
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
                        self.chunk.write_u8(Op::DefineGlobal.into(), span);
                        self.chunk.write_u8(constant, span);
                    }
                } else {
                    return Err("Too many constants".to_string());
                }
                // }
            }
            (Operator::Equal, [Tree::Atom(Atom::Identifier(name), _), value]) => {
                let (arg, _, set_op) = match self.get_variable(name) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.compile_tree(&value)?;
                self.chunk.write_u8(set_op.into(), span);
                self.chunk.write_u8(arg, span);
            }
            (op, _) => todo!("Implement operator in compiler.rs {op:?}"),
        }
        Ok(())
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
            self.chunk.write_u8(Op::PopN.into(), &(0..0).into());
            self.chunk.write_u8(pops as u8, &(0..0).into());
            self.locals.truncate(keep);
        }
    }
}
