//! The source module provides the `Source` struct, and all the tools needed to work with source files.

use std::path::PathBuf;

/// Represents a source file.
pub struct Source {
    /// Path to the source file.
    path: PathBuf,
    /// Contents of the source file.
    contents: String,
    /// Line offsets for the source file.
    line_offsets: Vec<usize>,
}

impl Source {
    /// Creates a new source from the given path.
    pub fn from_path<P: Into<PathBuf>>(path: P) -> Result<Self, std::io::Error> {
        let path = path.into();

        // Read the contents of the file.
        let contents = std::fs::read_to_string(&path)?;

        // Create a vector of line offsets. First line always starts at 0.
        let mut line_offsets = vec![0];

        // Find all newline characters and store their byte offsets.
        line_offsets.extend(contents.match_indices('\n').map(|(i, _)| i));

        Ok(Self {
            path,
            contents,
            line_offsets,
        })
    }

    /// Returns the path to the source file.
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Returns the contents of the source file.
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// Returns the line number and column for the given byte offset.
    pub fn source_position(&self, offset: usize) -> (usize, usize) {
        // Find the line index by searching for the first line offset that is greater than the offset.
        let idx = self
            .line_offsets
            .iter()
            .position(|&end| end > offset)
            .unwrap_or(self.line_offsets.len());

        // Get the start offset of the line and calculate the column by subtracting the start offset.
        let start = self
            .line_offsets
            .get(idx.saturating_sub(1))
            .copied()
            .unwrap();

        (idx, offset - start)
    }
}
