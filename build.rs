#[cfg(feature = "regen-tests")]
fn main() -> anyhow::Result<()> {
    use anyhow::Context;
    use std::fs::{read_dir, File};
    use std::io::{BufWriter, Write};

    let mut test_suite = BufWriter::new(File::create("tests/main.rs")?);
    writeln!(test_suite, "//! Auto-generated from build.rs\n\nmod common;\nuse common::cmp;\n")?;

    for entry in read_dir("tests/samples")? {
        let entry = entry?;
        let path = entry.path();
        let name = path.file_name().context("no filename")?.to_str().context("invalid filename")?;

        writeln!(test_suite, "#[test]\nfn {name}() {{\n    cmp({path:?})\n}}\n")?;
    }
    Ok(())
}

#[cfg(not(feature = "regen-tests"))]
fn main() {}
