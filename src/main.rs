//! The `zen` language is a simple, statically-typed, expression-based language.
//! This crate provides the implementation of the `zen` compiler.

#![deny(clippy::missing_docs_in_private_items)]
#![deny(missing_docs)]

use scanner::Scanner;
use source::{Source, SourceList};

pub mod scanner;
pub mod source;
pub mod token;

fn main() {
    let mut sources = SourceList::new();
    let id = sources.add_source(Source::from_path("examples/hello_world.zen").unwrap());

    let mut scanner = Scanner::new(sources.get_source(id).unwrap());

    loop {
        match scanner.scan_next() {
            Ok(None) => break,
            Ok(Some(token)) => println!("{:?}", token),
            Err(err) => eprintln!("{}", err),
        }
    }
}
