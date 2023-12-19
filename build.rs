#[cfg(feature = "regen-tests")]
fn main() {
    use std::fs::{read_dir, File};
    use std::io::{BufWriter, Write};

    let mut test_suite = BufWriter::new(File::create("tests/main.rs").unwrap());
    writeln!(
        test_suite,
        "//! Auto-generated from build.rs\n\nmod common;\nuse common::cmp;\n"
    )
    .unwrap();

    for entry in read_dir("tests/samples").unwrap() {
        let entry = entry.unwrap();
        let mut src = entry.path();
        let name = src.file_name().unwrap().to_str().unwrap().to_owned();
        src.push("source.rs");
        let dst = src.with_file_name("target.rs");

        writeln!(
            test_suite,
            "#[test] fn {name}() {{\n\tcmp({src:?}, {dst:?})\n}}\n"
        )
        .unwrap();
    }
}

#[cfg(not(feature = "regen-tests"))]
fn main() {}
