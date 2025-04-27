use std::{borrow::Cow, cell::RefCell, rc::Rc};

use crate::{
    error::{CompilerError, Error, Span},
    syntax::{
        ast::{Atom, Operator, Tree},
        parse::Parser,
    },
};

use super::{
    chunk::Chunk, function::Function, op::Op, value::Value
};

pub(crate) struct Compiler<'a> {
    parser: Parser<'a>,
    chunk: RefCell<Chunk>,
    locals: RefCell<Vec<Local<'a>>>,
    scope_depth: usize,
}

struct Local<'a> {
    name: Cow<'a, str>,
    depth: Option<usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chunk: RefCell::new(Chunk::new()),
            parser: Parser::new(source),
            locals: RefCell::new(Vec::new()),
            scope_depth: 0,
        }
    }

    pub(crate) fn compile(mut self) -> Result<Function, Error> {
        self.locals.borrow_mut().push(Local {
             name: Cow::Borrowed(""),
             depth: Some(0),
         });

        let ast = self.parser.parse()?;

        self.compile_tree(&ast)?;
        self.emit_op(Op::Return, &(0..0).into());
        let chunk = self.chunk.replace(Chunk::new());
        Ok(Function::new(&Rc::new(chunk)))
    }

    fn compile_tree(&mut self, tree: &Tree<'a>) -> Result<(), Error> {
        match tree {
            Tree::Atom(atom, span) => self.compile_atom(atom, span),
            Tree::Construct(op, args, span) => self.compile_cons(op, args, span),
        }
    }

    fn compile_atom(&mut self, atom: &Atom, span: &Span) -> Result<(), Error> {
        match atom {
            Atom::Float(n) => {
                let constant = self.write_constant(Value::Float(*n))?;
                self.emit_op_with_args(Op::Constant, &[constant], span);
            }
            Atom::Int(n) => {
                let constant = self.write_constant(Value::Int(*n))?;
                self.emit_op_with_args(Op::Constant, &[constant], span);
            }
            Atom::Bool(b) => {
                let constant = self.write_constant(Value::Bool(*b))?;
                self.emit_op_with_args(Op::Constant, &[constant], span);
            }
            Atom::String(s) => {
                let constant = self.write_constant(Value::String(s.to_string()))?;
                self.emit_op_with_args(Op::Constant, &[constant], span);
            }
            Atom::Identifier(s) => {
                let (arg, get_op, _) = match self.get_variable(s) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.emit_op_with_args(get_op, &[arg], span);
            }
        }
        Ok(())
    }

    fn compile_cons(
        &mut self,
        op: &Operator,
        args: &[Tree<'a>],
        span: &Span,
    ) -> Result<(), Error> {
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
                    if self.locals.borrow().iter().filter(|x| x.name == *name).count() != 0 {
                        return Err(Error::CompilerError(CompilerError::VariableAlreadyDeclared {
                            name: name.to_string(),
                            span: span.clone().into(),
                        }));
                    } else {
                        let loc = Local {
                            name: name.clone(),
                            depth: None,
                        };
                        self.locals.borrow_mut().push(loc);
                        let last = self.locals.borrow().len() - 1;
                        self.locals.borrow_mut()[last].depth = Some(self.scope_depth);
                    }
                }

                let constant = self.write_constant(Value::String(name.to_string()))?;
                self.compile_tree(&value)?;
                if self.scope_depth == 0 {
                    self.emit_op_with_args(Op::DefineGlobal, &[constant], span);
                }

            }
            (Operator::Equal, [Tree::Atom(Atom::Identifier(name), _), value]) => {
                let (arg, _, set_op) = match self.get_variable(name) {
                    Ok(result) => result,
                    Err(_) => todo!("Error"),
                };
                self.compile_tree(&value)?;
                self.emit_op_with_args(set_op, &[arg], span);
            }
            (Operator::And, [left, right]) => {
                self.compile_tree(&left)?;
                let offset_jump = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(right)?;
                self.patch_jump(offset_jump);
            }
            (Operator::Or, [left, right]) => {
                self.compile_tree(&left)?;
                let else_jump = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                let end_jump = self.emit_op_with_args(Op::Jump, &[0xff, 0xff], span);

                self.patch_jump(else_jump);
                self.emit_op(Op::Pop, span);

                self.compile_tree(&right)?;
                self.patch_jump(end_jump);
            }
            (Operator::If, [condition, then]) => {
                self.compile_tree(condition)?;
                let then_offset = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(then)?;
                self.patch_jump(then_offset);
            }
            (Operator::If, [condition, then, else_]) => {
                self.compile_tree(condition)?;
                let then_offset = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(&then)?;

                let else_offset = self.emit_op_with_args(Op::Jump, &[0xff, 0xff], span);
                self.patch_jump(then_offset);
                self.emit_op(Op::Pop, span);

                self.compile_tree(else_)?;
                self.patch_jump(else_offset);
            }
            (Operator::While, [condition, then]) => {
                let loop_start = self.chunk.borrow().count();
                self.compile_tree(condition)?;

                let jump_offset = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);

                self.compile_tree(then)?;
                let offset = self.chunk.borrow().count() + 3 - loop_start;
                if offset > u16::MAX as usize {
                    return Err(Error::CompilerError(CompilerError::LoopBodyToLarge));
                }
                self.emit_op_with_args(
                    Op::Loop,
                    &[((offset >> 8) & 0xff) as u8, (offset & 0xff) as u8],
                    span,
                );

                self.patch_jump(jump_offset);
                self.emit_op(Op::Pop, span);
            }
            (Operator::Loop, [body]) => {
                let loop_start = self.chunk.borrow().count();
                self.compile_tree(body)?;

                let offset = self.chunk.borrow().count() + 3 - loop_start;
                if offset > u16::MAX as usize {
                    return Err(Error::CompilerError(CompilerError::LoopBodyToLarge));
                }
                self.emit_op_with_args(
                    Op::Loop,
                    &[((offset >> 8) & 0xff) as u8, (offset & 0xff) as u8],
                    span,
                );
            }
            (op, _) => todo!("Implement operator in compiler.rs {op:?}"),
        }
        Ok(())
    }


    fn write_constant(&mut self, value: Value) -> Result<u8, Error> {
        if let Some(constant) = self.chunk.borrow_mut().write_constant(value) {
            return Ok(constant);
        }
        Err(Error::CompilerError(CompilerError::TooManyConstants))
    }

    /// Emits an operation to the chunk and returns the index of the operation
    fn emit_op(&mut self, op: Op, span: &Span) -> usize {
        self.emit_op_with_args(op, &[], span)
    }

    /// Emits an operation with multiple arguments to the chunk and returns the index of the operation
    fn emit_op_with_args(&mut self, op: Op, args: &[u8], span: &Span) -> usize {
        self.chunk.borrow_mut().write_u8(op.into(), span);
        let offset = self.chunk.borrow().count();
        for &arg in args {
            self.chunk.borrow_mut().write_u8(arg, span);
        }

        offset
    }

    fn patch_jump(&mut self, offset: usize) {
        // TODO: In crafting interpreters, they do this in two parts because they have a single pass compiler.
        //       We _could_ compile a tree first then count the bytes and only apply it once we have the final size.
        let relative = self.chunk.borrow().count() - offset - 2;
        if relative > u16::MAX as usize {
            panic!("Too much code to jump over.");
        }
        self.chunk.borrow_mut().update_u8(offset, ((relative >> 8) & 0xff) as u8);
        self.chunk.borrow_mut().update_u8(offset + 1, (relative & 0xff) as u8);
    }

    fn get_variable(&mut self, name: &Cow<str>) -> Result<(u8, Op, Op), ()> {
        if let Some(local_arg) = self.resolve_local(name) {
            return Ok((local_arg, Op::GetLocal, Op::SetLocal));
        } else if let Some(global_arg) = self.chunk.borrow_mut().write_constant(Value::String(name.to_string()))
        {
            return Ok((global_arg, Op::GetGlobal, Op::SetGlobal));
        }
        Err(())
    }

    fn resolve_local(&self, name: &Cow<str>) -> Option<u8> {
        for (e, v) in self.locals.borrow().iter().rev().enumerate() {
            if v.name == *name {
                if v.depth.is_none() {
                    todo!("Can't read local variable in its own initializer.");
                }
                return Some((self.locals.borrow().len() - e - 1) as u8);
            }
        }
        None
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        let mut keep = self.locals.borrow().len();
        while keep > 0 && self.locals.borrow()[keep - 1].depth.unwrap() > self.scope_depth {
            keep -= 1;
        }
        let pops = self.locals.borrow().len() - keep;
        if pops > 0 {
            self.emit_op_with_args(Op::PopN, &[pops as u8], &(0..0).into());
            self.locals.borrow_mut().truncate(keep);
        }
    }
}
