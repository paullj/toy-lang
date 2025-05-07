use std::{borrow::Cow, mem};

use crate::{
    error::{CompilerError, Error, Span},
    syntax::{
        ast::{Atom, Operator, Tree},
        parse::{NO_SPAN, Parser},
    },
};

use super::{function::Function, op::Op, value::Value};

pub(crate) struct Compiler<'a> {
    parser: Parser<'a>,
    context: Context,
}

#[derive(Debug)]
struct Context {
    function: Function,
    locals: Vec<Local>,
    scope_depth: usize,
    parent: Option<Box<Context>>,
}

impl Context {
    fn new(function: Function, scope_depth: usize) -> Self {
        Self {
            function,
            scope_depth,
            locals: Vec::new(),
            parent: None,
        }
    }
    fn script() -> Self {
        Self::new(Function::new("script".into(), 0), 0)
    }
}

impl Context {
    fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    fn resolve_local(&self, name: &Cow<str>) -> Option<u8> {
        for (i, local) in self.locals.iter().rev().enumerate() {
            if local.name == *name {
                if local.depth.is_none() {
                    todo!("Can't read local variable in its own initializer.");
                }
                return Some((self.locals.len() - i - 1) as u8);
            }
        }
        None
    }
}

#[derive(Debug, Default)]
struct Local {
    name: String,
    depth: Option<usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            parser: Parser::new(source),
            context: Context::script(),
        }
    }

    pub(crate) fn compile(mut self) -> Result<Function, Error> {
        let root = self.parser.parse()?;
        self.compile_tree(&root)?;

        self.emit_op(Op::Return, &NO_SPAN);

        Ok(self.context.function)
    }

    fn compile_tree(&mut self, tree: &Tree<'a>) -> Result<(), Error> {
        match tree {
            Tree::Atom(atom, span) => self.compile_atom(atom, span),
            Tree::Construct(op, args, span) => self.compile_construct(op, args, span),
        }
    }

    fn compile_atom(&mut self, atom: &Atom, span: &Span) -> Result<(), Error> {
        let mut emit_value = |value: Value| -> Result<(), Error> {
            let constant = self.write_constant(value)?;
            self.emit_op_with_args(Op::Constant, &[constant], span);
            Ok(())
        };

        match atom {
            Atom::Float(n) => emit_value(Value::Float(*n))?,
            Atom::Int(n) => emit_value(Value::Int(*n))?,
            Atom::Bool(b) => emit_value(Value::Bool(*b))?,
            Atom::String(s) => emit_value(Value::String(s.to_string()))?,
            Atom::Identifier(s) => {
                let (arg, get_op, _) = match self.get_variable(s) {
                    Ok(result) => result,
                    Err(_) => todo!("error in get_variable"),
                };
                self.emit_op_with_args(get_op, &[arg], span);
            }
        }
        Ok(())
    }

    fn compile_construct(
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
            (Operator::Block, args) => {
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
                if !self.context.is_global() {
                    if self
                        .context
                        .locals
                        .iter()
                        .filter(|x| x.name == *name)
                        .count()
                        != 0
                    {
                        return Err(Error::CompilerError(
                            CompilerError::VariableAlreadyDeclared {
                                name: name.to_string(),
                                span: span.clone().into(),
                            },
                        ));
                    } else {
                        self.context.locals.push(Local {
                            name: name.to_string(),
                            depth: None,
                        });
                        if let Some(last) = self.context.locals.last_mut() {
                            last.depth = Some(self.context.scope_depth);
                        }
                    }
                }

                let constant = self.write_constant(Value::String(name.to_string()))?;
                self.compile_tree(&value)?;
                if self.context.is_global() {
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
            (Operator::If, [condition, then @ Tree::Construct(Operator::Block, _, _)]) => {
                self.compile_tree(condition)?;
                let then_offset = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);
                self.compile_tree(then)?;
                self.patch_jump(then_offset);
            }
            (
                Operator::If,
                [
                    condition,
                    then @ Tree::Construct(Operator::Block, _, _),
                    else_ @ Tree::Construct(Operator::Block, _, _),
                ],
            ) => {
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
            (Operator::While, [condition, then @ Tree::Construct(Operator::Block, _, _)]) => {
                let loop_start = self.context.function.chunk.size();
                self.compile_tree(condition)?;

                let jump_offset = self.emit_op_with_args(Op::JumpIfFalse, &[0xff, 0xff], span);
                self.emit_op(Op::Pop, span);

                self.compile_tree(then)?;
                let offset = self.context.function.chunk.size() + Op::Loop.size() - loop_start;
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
            (Operator::Loop, [body @ Tree::Construct(Operator::Block, _, _)]) => {
                let loop_start = self.context.function.chunk.size();
                self.compile_tree(body)?;

                let offset = self.context.function.chunk.size() + Op::Loop.size() - loop_start;
                if offset > u16::MAX as usize {
                    return Err(Error::CompilerError(CompilerError::LoopBodyToLarge));
                }
                self.emit_op_with_args(
                    Op::Loop,
                    &[((offset >> 8) & 0xff) as u8, (offset & 0xff) as u8],
                    span,
                );
            }
            (
                Operator::Fn,
                [
                    Tree::Atom(Atom::Identifier(name), _),
                    args @ ..,
                    body @ Tree::Construct(Operator::Block, _, _),
                ],
            ) if args
                .iter()
                .all(|e| matches!(e, Tree::Atom(Atom::Identifier(_), _))) =>
            {
                let function = Function::new(name.to_string(), args.len() as u8);
                self.begin_context(Context::new(function.clone(), self.context.scope_depth + 1));
                self.begin_scope();

                self.context.locals.push(Local {
                    name: name.to_string(),
                    depth: Some(self.context.scope_depth),
                });

                for arg in args {
                    if let Tree::Atom(Atom::Identifier(name), _) = arg {
                        self.context.locals.push(Local {
                            name: name.to_string(),
                            depth: Some(self.context.scope_depth),
                        });
                    }
                }
                self.compile_tree(body)?;
                self.emit_op(Op::Return, span);

                self.end_context();

                let constant = self.write_constant(Value::Function(function))?;
                self.emit_op_with_args(Op::Constant, &[constant], span);
                self.emit_op_with_args(Op::DefineGlobal, &[constant], span);
            }
            (op, _) => todo!("Implement operator in compiler.rs {op:?}"),
        }
        Ok(())
    }

    fn write_constant(&mut self, value: Value) -> Result<u8, Error> {
        if let Some(index) = self.context.function.chunk.write_constant(value) {
            return Ok(index);
        }
        Err(Error::CompilerError(CompilerError::TooManyConstants))
    }

    /// Emits an operation to the chunk and returns the index of the operation
    fn emit_op(&mut self, op: Op, span: &Span) -> usize {
        self.emit_op_with_args(op, &[], span)
    }

    /// Emits an operation with multiple arguments to the chunk and returns the index of the operation
    fn emit_op_with_args(&mut self, op: Op, args: &[u8], span: &Span) -> usize {
        self.context.function.chunk.write_u8(op.into(), span);
        let offset = self.context.function.chunk.size();
        for &arg in args {
            self.context.function.chunk.write_u8(arg, span);
        }

        offset
    }

    fn patch_jump(&mut self, offset: usize) {
        // TODO: In crafting interpreters, they do this in two parts because they have a single pass compiler.
        //       We _could_ compile a tree first then count the bytes and only apply it once we have the final size.
        let relative = self.context.function.chunk.size() - offset - 2;
        if relative > u16::MAX as usize {
            panic!("Too much code to jump over.");
        }
        self.context
            .function
            .chunk
            .update_u8(offset, ((relative >> 8) & 0xff) as u8);
        self.context
            .function
            .chunk
            .update_u8(offset + 1, (relative & 0xff) as u8);
    }

    fn get_variable(&mut self, name: &Cow<str>) -> Result<(u8, Op, Op), ()> {
        if let Some(local_arg) = self.context.resolve_local(name) {
            return Ok((local_arg, Op::GetLocal, Op::SetLocal));
        } else if let Some(global_arg) = self
            .context
            .function
            .chunk
            .write_constant(Value::String(name.to_string()))
        {
            return Ok((global_arg, Op::GetGlobal, Op::SetGlobal));
        }
        Err(())
    }

    fn begin_context(&mut self, context: Context) {
        let previous = mem::replace(&mut self.context, context);
        self.context.parent = Some(Box::new(previous));
    }

    fn end_context(&mut self) -> Context {
        let parent = self
            .context
            .parent
            .take()
            .expect("tried to end context in a script");
        let context = mem::replace(&mut self.context, *parent);
        context
    }

    fn begin_scope(&mut self) {
        self.context.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.context.scope_depth -= 1;

        let keep = self.context.locals.len()
            - self
                .context
                .locals
                .iter()
                .rev()
                .take_while(|local| local.depth.unwrap() > self.context.scope_depth)
                .count();
        let pops = self.context.locals.len() - keep;
        if pops > 0 {
            self.emit_op_with_args(Op::PopN, &[pops as u8], &NO_SPAN);
            self.context.locals.truncate(keep);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile() {
        let source = "let x = 1 + 2";
        let compiler = Compiler::new(source);
        let result = compiler.compile();
        assert!(result.is_ok());

        let function = result.unwrap();
        let chunk = function.chunk;

        assert_eq!(chunk.size(), 8, "Expected 8 ops, got {}", chunk.size());
        assert_eq!(
            chunk.read_u8(0).unwrap(),
            Op::Constant.into(),
            "Expected Op::Constant got {}",
            Op::from(chunk.read_u8(0).unwrap())
        );
        assert_eq!(
            chunk.read_u8(1).unwrap(),
            1,
            "Expected 1, got {}",
            chunk.read_u8(1).unwrap()
        );
        assert_eq!(
            chunk.read_u8(2).unwrap(),
            Op::Constant.into(),
            "Expected Op::Constant got {}",
            Op::from(chunk.read_u8(2).unwrap())
        );
        assert_eq!(
            chunk.read_u8(3).unwrap(),
            2,
            "Expected 2, got {}",
            chunk.read_u8(3).unwrap()
        );
        assert_eq!(
            chunk.read_u8(4).unwrap(),
            Op::Add.into(),
            "Expected Op::Add got {}",
            Op::from(chunk.read_u8(4).unwrap())
        );
        assert_eq!(
            chunk.read_u8(5).unwrap(),
            Op::DefineGlobal.into(),
            "Expected Op::DefineGlobal got {}",
            Op::from(chunk.read_u8(5).unwrap())
        );
        assert_eq!(
            chunk.read_u8(6).unwrap(),
            0,
            "Expected 0, got {}",
            chunk.read_u8(6).unwrap()
        );
        assert_eq!(
            chunk.read_u8(7).unwrap(),
            Op::Return.into(),
            "Expected Op::Return got {}",
            Op::from(chunk.read_u8(8).unwrap())
        );
    }
}
