#![allow(dead_code)]

mod ast;
mod file_cache;
mod parse;
mod pass;
mod report_error;
//mod runtime;
mod util;

#[cfg(test)]
mod tests;

fn main() {
    use file_cache::FileCache;
    use pass::*;
    use report_error::report_error as report;
    use std::fs;

    let mut files = FileCache::new();
    let input = "test.text";
    let output = "test.rs";

    let resolved = match resolve::resolve(&mut files, input) {
        Ok(resolved) => resolved,
        Err(err) => {
            // TODO: do not unwrap
            report(files.read(input).unwrap(), err);
            return;
        }
    };

    let typed = type_infer::infer(resolved);
    let lifted = lam_lift::lift(typed);

    let file = fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(output)
        .unwrap();
    let lowered = lower::lower(&lifted, file).unwrap();
}
