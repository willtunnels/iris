use lalrpop_util::lalrpop_mod;
lalrpop_mod!(parse);

/// Parses the input string into raw_ast program
fn parse_program(s: String) {
    parse::ProgramParser::new().parse(s).unwrap();
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_raw() {
        //assert something here...
    }
}
