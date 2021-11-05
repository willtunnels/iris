use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ByteIdx(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LineIdx(pub usize);

#[derive(Clone, Debug)]
pub struct File {
    text: String,
    line_starts: Vec<ByteIdx>,
}

impl File {
    fn new(text: String) -> Self {
        let mut line_starts = vec![ByteIdx(0)];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(ByteIdx(i + 1));
            }
        }
        Self { text, line_starts }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn line(&self, line_idx: LineIdx) -> &str {
        let start = self.line_starts[line_idx.0].0;
        let end = if let Some(end) = self.line_starts.get(line_idx.0 + 1) {
            end.0
        } else {
            self.text.len()
        };

        let line = &self.text[start..end];
        if line.ends_with('\r') {
            &line[..line.len() - 1]
        } else {
            line
        }
    }

    pub fn line_idx(&self, byte_idx: ByteIdx) -> LineIdx {
        match self.line_starts.binary_search(&byte_idx) {
            Ok(line) => LineIdx(line),
            Err(next_line) => {
                debug_assert!(next_line > 0);
                LineIdx(next_line - 1)
            }
        }
    }

    pub fn col_idx(&self, line_idx: LineIdx, byte_idx: ByteIdx) -> usize {
        byte_idx.0 - self.line_starts[line_idx.0].0
    }
}

#[derive(Clone, Debug)]
pub struct FileCache {
    files: HashMap<PathBuf, File>,
}

impl FileCache {
    pub fn new() -> Self {
        FileCache {
            files: HashMap::new(),
        }
    }

    pub fn read(&mut self, path: impl AsRef<Path>) -> io::Result<&File> {
        let canonical = path.as_ref().canonicalize()?;
        if !self.files.contains_key(&canonical) {
            let text = fs::read_to_string(&canonical)?;
            self.files.insert(canonical.clone(), File::new(text));
        }
        Ok(&self.files[&canonical])
    }

    pub fn read_cached(&self, path: impl AsRef<Path>) -> io::Result<&File> {
        let canonical = path.as_ref().canonicalize()?;
        Ok(&self.files[&canonical])
    }
}
