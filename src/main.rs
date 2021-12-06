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
    let mut files = file_cache::FileCache::new();
    let err = report_error::Locate {
        kind: "What I find remarkable is that this text has been the industry's standard dummy text ever since some printer in the 1500s took a galley of type and scrambled it to make a type specimen book; it has survived not only four centuries of letter-by-letter resetting but even the leap into electronic typesetting, essentially unchanged except for an occasional 'ing' or 'y' thrown in. It's ironic that when the then-understood Latin was scrambled, it became as incomprehensible as Greek; the phrase 'it's Greek to me' and 'greeking' have common semantic roots!” (The editors published his letter in a correction headlined “Lorem Oopsum”).",
        span: Some(ast::Span(3, 16)),
    };
    report_error::report_error(files.read("test.txt").unwrap(), err);
}
