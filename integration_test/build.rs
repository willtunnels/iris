fn main() {
    burnt_toast::Config::new()
        .emit_rerun_directives(true)
        .compile("inc.txt");

    println!("cargo:rerun-if-changed=build.rs");
}
