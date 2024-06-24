//! This module contains the command-line interface for the `zen` compiler.

use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "zen")]
#[command(version = env!("VERSION"))]
#[command(about = "The `zen` language compiler.")]
/// The command-line interface for the `zen` compiler.
pub enum Command {
    /// Compiles a source file and runs the resulting program.
    Run(BuildOptions),
    /// Compiles a source file.
    Build(BuildOptions),
}

/// The build options for the `zen` compiler.
#[derive(Debug, Parser)]
pub struct BuildOptions {
    /// The path to the source file.
    pub path: PathBuf,

    /// Whether to build the program in release mode.
    #[arg(short, long)]
    pub release: bool,
}
