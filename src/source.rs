//! The source module provides the `Source` struct, and all the tools needed to work with source files.

use std::{collections::HashMap, fmt::Debug, path::PathBuf};

#[derive(Debug, Clone)]
/// Represents a source file.
pub struct Source {
    /// Path to the source file.
    path: PathBuf,
    /// Contents of the source file.
    contents: String,
    /// Line offsets for the source file.
    line_offsets: Vec<usize>,
    /// The id of the source file.
    id: SourceId,
}

impl Source {
    /// Creates a new source from the given path.
    pub fn from_path<P: Into<PathBuf>>(path: P) -> Result<Self, std::io::Error> {
        let path = path.into();

        // Read the contents of the file.
        let contents = std::fs::read_to_string(&path)?;

        Ok(Self::from_string(path, contents))
    }

    /// Creates a new source from the given path and contents.
    pub fn from_string<P: Into<PathBuf>, S: ToString>(path: P, contents: S) -> Self {
        let path = path.into();
        let contents = contents.to_string();

        // Create a vector of line offsets. First line always starts at 0.
        let mut line_offsets = vec![0];

        // Find all newline characters and store their byte offsets.
        line_offsets.extend(contents.match_indices('\n').map(|(i, _)| i));

        Self {
            path,
            contents,
            line_offsets,
            id: SourceId::INVALID,
        }
    }

    /// Returns the path to the source file.
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Returns the contents of the source file.
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// Returns the id of the source file.
    pub fn id(&self) -> SourceId {
        self.id
    }

    /// Sets the id of the source file.
    pub fn set_id<S: Into<SourceId>>(&mut self, id: S) {
        self.id = id.into();
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

#[derive(Clone, Copy)]
/// Identifier for a source file. Used to retrieve the source file from the source map.
pub struct SourceId(pub usize);

impl SourceId {
    /// Constant for an invalid source id.
    pub const INVALID: SourceId = SourceId(usize::MAX);
}

impl Debug for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == usize::MAX {
            write!(f, "SourceId::INVALID")
        } else {
            write!(f, "SourceId({})", self.0)
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Represents a span in the source file.
pub struct Span {
    /// The start offset of the span.
    pub start: usize,
    /// The end offset of the span.
    pub end: usize,
    /// The source file of the span.
    pub source_id: SourceId,
}

impl Span {
    /// Creates a new span from the given start and end offsets.
    pub fn new(start: usize, end: usize, source_id: SourceId) -> Self {
        Self {
            start,
            end,
            source_id,
        }
    }
}

#[derive(Debug)]
/// Represents a list of source files.
pub struct SourceList {
    /// List of source files.
    sources: Vec<Source>,
    /// Map of source files to their ids.
    source_map: HashMap<PathBuf, SourceId>,
}

impl SourceList {
    /// Creates a new source list.
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
            source_map: HashMap::new(),
        }
    }

    /// Adds a source file to the list.
    pub fn add_source(&mut self, mut source: Source) -> SourceId {
        let path = source.path().clone();
        let id = SourceId(self.sources.len());

        // Set the id of the source file.
        source.set_id(id);

        self.sources.push(source);
        self.source_map.insert(path, id);

        id
    }

    /// Returns the source file for the given id.
    pub fn get_source(&self, id: SourceId) -> Option<&Source> {
        self.sources.get(id.0)
    }

    /// Returns the source for the given path.
    pub fn get_source_by_path(&self, path: &PathBuf) -> Option<&Source> {
        let id = self.source_map.get(path)?;
        self.sources.get(id.0)
    }
}
