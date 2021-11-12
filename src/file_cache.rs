use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LinePos(pub usize);

#[derive(Clone, Debug)]
pub struct File {
    text: String,
    line_starts: Vec<BytePos>,
}

fn find_line_starts(text: &str) -> Vec<BytePos> {
    let mut line_starts = vec![BytePos(0)];
    for (i, c) in text.char_indices() {
        if c == '\n' {
            line_starts.push(BytePos(i + 1));
        }
    }
    line_starts
}

impl File {
    fn new(text: String) -> Self {
        let line_starts = find_line_starts(&text);
        Self { text, line_starts }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn line(&self, line_pos: LinePos) -> &str {
        let start = self.line_starts[line_pos.0].0;
        let end = if let Some(end) = self.line_starts.get(line_pos.0 + 1) {
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

    pub fn line_pos(&self, byte_pos: BytePos) -> LinePos {
        match self.line_starts.binary_search(&byte_pos) {
            Ok(line) => LinePos(line),
            Err(next_line) => {
                debug_assert!(next_line > 0);
                LinePos(next_line - 1)
            }
        }
    }

    pub fn col_pos(&self, line_pos: LinePos, byte_pos: BytePos) -> usize {
        byte_pos.0 - self.line_starts[line_pos.0].0
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
