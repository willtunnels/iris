#![allow(dead_code)]

mod ast;
mod builtins;
mod file_cache;
mod parse;
mod pass;
mod report_error;
mod util;

pub mod runtime;

#[cfg(test)]
mod tests;

#[macro_export]
macro_rules! iris_mod {
    ($(#[$attr:meta])* $vis:vis $mod:ident) => {
        iris_mod!($(#[$attr])* $vis $mod, concat!("/", stringify!($mod), ".rs"));
    };

    ($(#[$attr:meta])* $vis:vis $mod:ident, $src:expr) => {
        $(#[$attr])* $vis mod $mod { include!(concat!(env!("OUT_DIR"), $src)); }
    };
}

pub struct Config {
    emit_rerun_directives: bool,
}

impl Config {
    pub fn new() -> Self {
        Self {
            emit_rerun_directives: false,
        }
    }

    pub fn emit_rerun_directives(&mut self, val: bool) -> &mut Self {
        self.emit_rerun_directives = val;
        self
    }

    pub fn compile(&self, input: &str) {
        use std::{env, fs, path::Path, process::Command};

        if self.emit_rerun_directives {
            println!("cargo:rerun-if-changed={}", input);
        }

        let name = Path::new(input).file_stem().unwrap().to_str().unwrap();
        let output = format!("{}/{}.rs", env::var("OUT_DIR").unwrap(), name);

        // If compilation does not output a file but also does not panic because of a bug, then we
        // don't want to leave an old version of the output for which compilation succeeded lying
        // around.
        if Path::new(&output).exists() {
            fs::remove_file(&output).unwrap();
        }

        compile(input, &output);

        if let Ok(rustfmt) = which::which("rustfmt") {
            Command::new(rustfmt).arg(&output).spawn().unwrap();
        }
    }
}

fn compile(input: &str, output: &str) {
    use file_cache::FileCache;
    use pass::*;
    use report_error::report_error as report;
    use std::fs;

    let mut files = FileCache::new();
    let resolved = match resolve::resolve(&mut files, input) {
        Ok(resolved) => resolved,
        Err(err) => panic!("{}", report(files.read(input).unwrap(), err)),
    };

    let typed = type_infer::infer(resolved);
    let lifted = lam_lift::lift(typed);

    let file = fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(output)
        .unwrap();
    lower::lower(&lifted, file).unwrap();
}
