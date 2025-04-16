mod error;
mod machine;
mod syntax;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use machine::Machine;
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

    #[command(subcommand)]
    cmd: Option<Commands>,
}

#[derive(Subcommand, Debug, Clone)]
enum Commands {
    Lex { source: String },
    Parse { source: String },
}

use miette::{Diagnostic, Report};
use thiserror::Error;

// TODO: Move this into error module and make it more generic for a collections of errors
#[derive(Diagnostic, Debug, Error)]
#[error("errors occurred while lexing")]
#[diagnostic()]
pub struct LexingError {
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
            Commands::Lex { source: _ } => {}
            // Commands::Lex { source } => {
            //     let lexer = Lexer::new(source);

            //     let (tokens, errors): (Vec<_>, Vec<_>) = lexer.partition(Result::is_ok);
            //     let errors: Vec<_> = errors.into_iter().map(Result::unwrap_err).collect();
            //     if !errors.is_empty() {
            //         let report: Report = LexingError {
            //             source_code: source.to_string(),
            //             errors,
            //         }
            //         .into();
            //         return Err(report);
            //     }

            //     let tokens: Vec<_> = tokens
            //         .into_iter()
            //         .map(Result::unwrap)
            //         .map(|(token, _)| token)
            //         .collect();

            //     println!(
            //         "Tokens: {}",
            //         tokens
            //             .iter()
            //             .map(|t| format!("{:?}", t))
            //             .collect::<Vec<_>>()
            //             .join(", ")
            //     );
            //     return Ok(ExitCode::SUCCESS);
            // }
            Commands::Parse { source } => {
                let mut parser = Parser::new(source);
                let result = parser.parse();

                match result {
                    Ok(ast) => println!("AST: {}", ast.to_string()),
                    Err(e) => {
                        return Err(e.with_source_code(source.to_string()));
                    }
                }
            }
        }
    }

    match args.path {
        Some(path) => {
            todo!("Run file {}", path.display());
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
                let mut machine = Machine::new();
                match editor.add_history_entry(line.as_str()) {
                    Ok(_) => {}
                    Err(e) => todo!("Failed to add history entry: {}", e),
                }
                let result = machine.interpret(line.as_str());
                match result {
                    Ok(_) => continue,
                    Err(e) => return Err(e.into()),
                }
            }
        }
    }

    Ok(ExitCode::SUCCESS)
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
