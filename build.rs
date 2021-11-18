fn main() {
    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .process_current_dir()
        .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}
