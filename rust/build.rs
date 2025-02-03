use std::env;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    Command::new("make")
        .env("DUNE_BUILD", out_dir.clone())
        .arg("build")
        .status()
        .unwrap();

    println!("cargo::rustc-link-search=native={}/install/default/lib/faial", out_dir);
    println!("cargo::rustc-link-lib=dylib=drf_api");
}
