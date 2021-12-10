use crate::ast::Span;
use crate::file_cache::{BytePos, File, LinePos};
use owo_colors::OwoColorize;
use std::fmt;

#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Locate<K> {
    pub kind: K,
    pub span: Option<Span>,
}

impl<K> From<K> for Locate<K> {
    fn from(kind: K) -> Self {
        Self { kind, span: None }
    }
}

fn digits(x: usize) -> usize {
    if x == 0 {
        1
    } else {
        ((x as f64).log10().floor() as usize) + 1
    }
}

fn snippet(file: &File, span: Span) -> String {
    let lo = file.line_pos(BytePos(span.0)).0;
    let hi = file.line_pos(BytePos(span.1)).0;
    let width = std::cmp::max(digits(lo), digits(hi));

    let mut text = String::new();
    for num in lo..=hi {
        let content_runner = format!("{num:>width$} | ", num = num, width = width);
        let content = file.line(LinePos(num));

        // Ensures the last line in a file is treated consistently, even if it doesn't have a '\n'
        let content = if content.ends_with("\n") {
            &content[..content.len() - 1]
        } else {
            content
        };

        text.push_str(&format!("{}{}\n", content_runner.blue().bold(), content));

        // If there is no content on this line, skip producing an extra line below it for carrots
        if content.len() == 0 {
            continue;
        }

        let len = content.len();
        let carrots_runner = format!("{} | ", " ".repeat(width));

        // The index math gets pretty finicky for where the " " ends and the "^" begins. Note that
        // the first column in a file is column 1, NOT column 0, and that we want carrots from byte
        // `span.0` to byte `span.1` INCLUSIVE.
        let carrots = if num == lo && num == hi {
            let col_lo = file.col_pos(LinePos(num), BytePos(span.0));
            let col_hi = file.col_pos(LinePos(num), BytePos(span.1));
            format!(
                "{}{}",
                " ".repeat(col_lo - 1),
                "^".repeat(col_hi - col_lo + 1)
            )
        } else if num == lo {
            let col = file.col_pos(LinePos(num), BytePos(span.0));
            format!("{}{}", " ".repeat(col - 1), "^".repeat(len - col + 1))
        } else if num == hi {
            let col = file.col_pos(LinePos(num), BytePos(span.1));
            format!("{}", "^".repeat(col + 1),)
        } else {
            format!("{}", "^".repeat(len))
        };

        text.push_str(&format!(
            "{}{}\n",
            carrots_runner.blue().bold(),
            carrots.red().bold()
        ));
    }

    text
}

pub fn report_error<K: fmt::Display>(file: &File, err: Locate<K>) -> String {
    use textwrap::{word_splitters::NoHyphenation, wrap, wrap_algorithms::OptimalFit, Options};
    let opt = Options::with_termwidth()
        .wrap_algorithm(OptimalFit)
        .word_splitter(NoHyphenation);
    let txt = format!("{}: {}", "error".red().bold(), &err.kind);

    // textwrap is smart enough to calculate display size in the presence of txt's ANSI escapes
    let msg = wrap(&txt, opt);

    let mut res = String::new();
    for line in msg {
        res.push_str(&line);
        res.push_str("\n");
    }

    res.push_str("\n");

    if let Some(span) = err.span {
        res.push_str(&snippet(file, span));
    }
    res
}
