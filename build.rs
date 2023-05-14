extern crate bindgen;
extern crate glob;
extern crate cc;

use glob::glob;
use std::env;
use std::ffi::OsStr;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let qbe_path = format!("{}/qbe", env::var("CARGO_MANIFEST_DIR").unwrap());
    let qbe_source_glob = format!("{}/*.c", qbe_path);
    let qbe_arch_glob = format!("{}/*64/*.c", qbe_path);

    let qbe_sources = glob(&qbe_source_glob).unwrap()
        .filter_map(Result::ok)
        .filter(|x| x.file_name() != Some(OsStr::new("main.c")));
    let qbe_arch = glob(&qbe_arch_glob).unwrap()
        .filter_map(Result::ok);
    cc::Build::new()
        .files(qbe_sources)
        .files(qbe_arch)
        .flag("-std=c99")
        .compile("qbe-sys");

    println!("cargo:rustc-link-search={}", out_dir.display());
    println!("cargo:rustc-link-lib=qbe-sys");

    let bindings = bindgen::Builder::default()
        .header(format!("{}/all.h", qbe_path).as_str())
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");
    bindings
        .write_to_file(out_dir.join("qbe-bindings.rs"))
        .expect("Couldn't write bindings");
}
