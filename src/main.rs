mod error;
mod syntax;

use codespan_reporting::{diagnostic::{Diagnostic, Label}, files::SimpleFile, term::{self, termcolor::{ColorChoice, StandardStream}}};
use error::{report_error, report_errors, SpannedError};
use syntax::{lex::{Lexer, SpannedToken, Token}, parse::Parser};

use std::{
    fs,
    io::{BufRead, Write},
    path::PathBuf,
    process::ExitCode,
};

use clap::{
    builder::styling::{AnsiColor, Color, Style}, Parser as ClapParser, Subcommand
};

/// a simple interpreter
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None, styles=get_styles())]
struct Arguments {
    /// Path of file to run
    #[arg(value_name = "PATH", index = 1)]
    path: Option<PathBuf>,

    #[command(subcommand)]
    cmd: Option<Commands>
}

#[derive(Subcommand, Debug, Clone)]
enum Commands {
    Lex{ contents: String },
    Parse{ contents: String },
}

fn main() -> ExitCode {
    let args = Arguments::parse();

    // Commands for testing lexing and parsing
    if let Some(cmd) = &args.cmd {
        match cmd {
            Commands::Lex { contents } => {
                let lexer = Lexer::new(contents);
                
                let errors: Vec<SpannedError> = lexer.clone().filter_map(Result::err).collect();
                
                if !errors.is_empty() {
                    let writer = StandardStream::stderr(ColorChoice::Always);
                    report_errors(&mut writer.lock(), contents, &errors);
                    return ExitCode::FAILURE;
                }

                let tokens: Vec<SpannedToken> = lexer.filter_map(Result::ok).collect();
                let diagnostic = Diagnostic::note()
                    .with_message("Lexing completed successfully")
                    .with_labels_iter(
                        tokens.iter().map(|(token, span)| {
                            Label::secondary((), span.clone())
                                .with_message(format!("{:?}", token))
                        })
                    );

                    let mut writer = StandardStream::stderr(ColorChoice::Always);
                    let file = SimpleFile::new("input", contents);
                    let config = term::Config::default();
                    term::emit(&mut writer, &config, &file, &diagnostic).expect("failed to write to output");
                return ExitCode::SUCCESS;
            }
            Commands::Parse { contents } => {
                let mut parser = Parser::new(contents);
                // TODO: support multiple errors
                match parser.parse() {
                    Ok(ast) => println!("{}", ast.to_string()),
                    Err(e) => {
                        eprintln!("Error: {:?}", e);
                        return ExitCode::FAILURE;
                    }
                }
            }
        }
        return ExitCode::SUCCESS;
    }

    if let Some(path) = args.path {
        match run_file(&path) {
            Ok(_) => {},
            Err(e) => {
                eprintln!("Error: {e}");
                return ExitCode::FAILURE;
            }
        }
    } else {
        match run_prompt() {
            Ok(_) => {},
            Err(e) => {
                eprintln!("Error: {e}");
                return ExitCode::FAILURE;
            }
        }
    }

    ExitCode::SUCCESS
}

fn run_file(path: &PathBuf) -> Result<(), String> {
    match fs::read_to_string(path) {
        Ok(contents) => run(&contents),
        Err(e) => Err(e.to_string()),
    }
}

fn run_prompt() -> Result<(), String> {
    loop {
        print!("> ");
        match std::io::stdout().flush() {
            Ok(_) => {},
            Err(e) => return Err(
                format!("Failed to flush stdout: {}", e.to_string())),
        }

        let mut input = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        match handle.read_line(&mut input) {
            Ok(0) | Ok(1) => return Ok(()), // EOF or empty line
            Err(e) => return Err(e.to_string()),
            _ => (),
        }
        let _ = run(&input);
    }
}

fn run(contents: &str) -> Result<(), String> {
    let mut parser = Parser::new(&contents);

    let reuslt = parser.parse();

    match reuslt {
        Ok(ast) => {
            println!("AST: {:#?}", ast);
        }
        Err(e) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            report_error(&mut writer.lock(), contents, &e);
        }
    }
    Ok(())
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
