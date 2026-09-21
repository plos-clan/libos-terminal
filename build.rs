fn main() {
    println!("cargo:rustc-check-cfg=cfg(embedded_font)");
    println!("cargo:rerun-if-env-changed=FONT_PATH");

    if std::env::var_os("FONT_PATH").is_some_and(|path| !path.is_empty()) {
        println!("cargo:rustc-cfg=embedded_font");
    }
}
