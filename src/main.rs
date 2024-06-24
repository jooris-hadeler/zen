//! The `zen` language is a simple, statically-typed, expression-based language.
//! This crate provides the implementation of the `zen` compiler.

#![deny(clippy::missing_docs_in_private_items)]
#![deny(missing_docs)]

use clap::Parser;
use cli::Command;
use scanner::Scanner;
use source::{Source, SourceList};

pub mod cli;
pub mod scanner;
pub mod source;
pub mod token;

fn main() {
    let command = Command::parse();

    match command {
        Command::Run(options) | Command::Build(options) => {
            let mut sources = SourceList::new();
            let id = sources.add_source(Source::from_path(options.path).unwrap());

            let scanner = Scanner::new(sources.get_source(id).unwrap());

            let tokens = scanner.collect();

            if let Some(tokens) = tokens {
                for token in tokens {
                    println!("{:?}", token);
                }
            }
        }
    }
}
