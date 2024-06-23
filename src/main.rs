//! The `zen` language is a simple, statically-typed, expression-based language.
//! This crate provides the implementation of the `zen` compiler.

#![deny(clippy::missing_docs_in_private_items)]
#![deny(missing_docs)]

use source::Source;

pub mod source;

fn main() {
    let hello_world_src = Source::from_path("examples/hello_world.zen").unwrap();

    println!("Path: {}", hello_world_src.path().display());
    println!("== Contents ==\n{}\n==============", hello_world_src.contents());

    println!(
        "Source position for offset 0: {:?}",
        hello_world_src.source_position(0)
    );
    println!(
        "Source position for offset 40: {:?}",
        hello_world_src.source_position(40)
    );
}
