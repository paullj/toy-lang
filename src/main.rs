mod error;
mod interpreter;
mod syntax;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

use interpreter::{machine::Machine, compiler::Compiler};
use syntax::{
    lex::{Lexer, Token},
    parse::Parser,
};

use std::{path::PathBuf, process::ExitCode};

use clap::{
    Parser as ClapParser, Subcommand,
    builder::styling::{AnsiColor, Color, Style},
};

/// a simple interpreter
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None, styles=get_styles())]
struct Arguments {
    /// Path of file to run
    #[arg(value_name = "PATH", index = 1)]
    path: Option<PathBuf>,

    #[arg(short, long)]
    debug: bool,

    #[command(subcommand)]
    cmd: Option<Commands>,
}

#[derive(Subcommand, Debug, Clone)]
enum Commands {
    /// Lex source into tokens
    #[command(name = "lex")]
    Lex { source: String },
    /// Parse source into an AST
    #[command(name = "parse")]
    Parse { source: String },
    /// Compile sourceinto bytecode
    #[command(name = "compile")]
    Compile { source: String },
}

use miette::{Diagnostic, Report};
use thiserror::Error;

// TODO: Move this into error module and make it more generic for a collections of errors
#[derive(Diagnostic, Debug, Error)]
#[error("An error occurred while lexing")]
#[diagnostic()]
pub struct Errors {
    #[source_code]
    source_code: String,
    #[related]
    errors: Vec<crate::error::Error>,
}

fn main() -> miette::Result<ExitCode> {
    let args = Arguments::parse();

    // Commands for testing lexing and parsing
    if let Some(cmd) = &args.cmd {
        match cmd {
            Commands::Lex { source } => {
                let contents = if PathBuf::from(source).exists() {
                    &std::fs::read_to_string(source).unwrap()
                } else {
                    &source
                };
                match lex(contents) {
                    Ok(_) => return Ok(ExitCode::SUCCESS),
                    Err(e) => return Err(e),
                }
            }
            Commands::Parse { source } => {
                let contents = if PathBuf::from(source).exists() {
                    &std::fs::read_to_string(source).unwrap()
                } else {
                    &source
                };
                match parse(contents) {
                    Ok(_) => return Ok(ExitCode::SUCCESS),
                    Err(e) => return Err(e),
                }
            }
            Commands::Compile { source } => {
                let contents = if PathBuf::from(source).exists() {
                    &std::fs::read_to_string(source).unwrap()
                } else {
                    &source
                };
                match compile(contents) {
                    Ok(_) => return Ok(ExitCode::SUCCESS),
                    Err(e) => return Err(e),
                }
            }
        }
    }

    match args.path {
        Some(path) => {
            run_file(&path, args.debug)
                .map_err(|e| miette::miette!("Failed to run file {}: {}", path.display(), e))?;
        }
        None => {
            let mut editor = match DefaultEditor::new() {
                Ok(editor) => editor,
                Err(e) => {
                    todo!("Failed to create editor: {}", e);
                }
            };
            loop {
                let line = match editor.readline(">>> ") {
                    Ok(line) => line,
                    Err(ReadlineError::Interrupted) => continue,
                    Err(ReadlineError::Eof) => break,
                    Err(e) => {
                        todo!("Failed to read line: {}", e);
                    }
                };
                match editor.add_history_entry(line.as_str()) {
                    Ok(_) => {}
                    Err(e) => todo!("Failed to add history entry: {}", e),
                }
                match run(line.as_str()) {
                    Ok(_) => continue,
                    Err(e) => return Err(e.into()),
                }
            }
        }
    }
    Ok(ExitCode::SUCCESS)
}

fn lex(source: &str) -> miette::Result<()> {
    let lexer = Lexer::new(source);

    let (tokens, errors): (Vec<_>, Vec<_>) = lexer.partition(Result::is_ok);
    let errors: Vec<_> = errors.into_iter().map(Result::unwrap_err).collect();
    if !errors.is_empty() {
        let report: Report = Errors {
            source_code: source.to_string(),
            errors,
        }
        .into();
        return Err(report);
    }

    let tokens: Vec<_> = tokens
        .into_iter()
        .map(Result::unwrap)
        .map(|(token, _)| token)
        .collect();

    println!(
        "{}",
        tokens
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<_>>()
            .join(", ")
    );
    Ok(())
}

fn parse(source: &str) -> miette::Result<()> {
    let mut parser = Parser::new(source);
    match parser.parse(){
        Ok(ast) => println!("{}", ast),
        Err(e) => {
            let report: miette::Report = e.into();
            return Err(report.with_source_code(source.to_string()));
        }
    }
    Ok(())
}

fn compile(source: &str) -> miette::Result<()> {
    let compiler = Compiler::new(source);
    match compiler.compile() {
        Ok(function) => {
            let chunk = function.chunk;
            let s = chunk.disassemble().unwrap();
            println!("{}", s);
            Ok(())
        }
        Err(e) => {
             let report: miette::Report = e.into();
            return Err(report.with_source_code(source.to_string()));
        }
    }
}

fn run_file(path: &PathBuf, debug: bool) -> miette::Result<()> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| miette::miette!("Failed to read file {}: {}", path.display(), e))?;

    if debug {
        // lex(source.as_str())?;
        // parse(source.as_str())?;
        compile(&source)?;
    }
    run(&source)
}

fn run(source: &str) -> miette::Result<()> {
    match Machine::interpret(source) {
        Ok(result) => {
            println!(
                "---\nProgram exited successfully. ({} + {})",
                format_duration(result.compilation),
                format_duration(result.execution)
            );
            Ok(())
        }
       Err(e) => {
             let report: miette::Report = e.into();
            return Err(report.with_source_code(source.to_string()));
        }
    }
}

fn format_duration(d: std::time::Duration) -> String {
    let ns = d.as_nanos();
    if ns < 1_000 {
        format!("{}ns", ns)
    } else if ns < 1_000_000 {
        format!("{:.2}µs", ns as f64 / 1_000.0)
    } else if ns < 1_000_000_000 {
        format!("{:.2}ms", ns as f64 / 1_000_000.0)
    } else if ns < 60_000_000_000 {
        format!("{:.2}s", ns as f64 / 1_000_000_000.0)
    } else if ns < 3600_000_000_000 {
        format!("{:.2}min", ns as f64 / 60_000_000_000.0)
    } else {
        format!("{:.2}hrs", ns as f64 / 3600_000_000_000.0)
    }
}

fn get_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .usage(title(AnsiColor::BrightWhite))
        .header(title(AnsiColor::BrightWhite))
        .literal(colored(AnsiColor::Cyan))
        .invalid(bold(AnsiColor::Red))
        .error(bold(AnsiColor::Red))
        .valid(title(AnsiColor::Green))
        .placeholder(colored(AnsiColor::White))
}

fn title(color: AnsiColor) -> Style {
    Style::new()
        .bold()
        .underline()
        .fg_color(Some(Color::Ansi(color)))
}

fn bold(color: AnsiColor) -> Style {
    Style::new().bold().fg_color(Some(Color::Ansi(color)))
}

fn colored(color: AnsiColor) -> Style {
    Style::new().fg_color(Some(Color::Ansi(color)))
}
