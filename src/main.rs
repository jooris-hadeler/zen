//! The `zen` language is a simple, statically-typed, expression-based language.
//! This crate provides the implementation of the `zen` compiler.

#![deny(clippy::missing_docs_in_private_items)]
#![deny(missing_docs)]

use clap::Parser as ClapParser;
use cli::Command;
use source::{Source, SourceList};
use syntax::parser::Parser;

pub mod cli;
pub mod scanner;
pub mod source;
pub mod syntax;
pub mod token;

fn main() {
    let command = Command::parse();

    match command {
        Command::Run(options) | Command::Build(options) => {
            let mut sources = SourceList::default();
            let id = sources.add_source(Source::from_path(options.path).unwrap());

            let mut parser = Parser::new(sources.get_source(id).unwrap());

            let module = parser.parse_module();
            println!("{module:#?}");
        }
    }
}
