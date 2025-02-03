use std::env;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=rust/build.rs");

    let out_dir = env::var("OUT_DIR").unwrap();

    let result = Command::new("make")
        .env("DUNE_BUILD", out_dir.clone())
        .arg("build")
        .status()
        .expect("Failed to execute make command");

    if !result.success() {
        panic!("Make command failed with status: {}", result);
    }

    println!("cargo::rustc-link-search=native={}/install/default/lib/faial", out_dir);
    println!("cargo::rustc-link-lib=static=drf_api");
}
