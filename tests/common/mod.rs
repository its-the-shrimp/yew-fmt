use std::fs::read;
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[allow(clippy::unwrap_used, clippy::expect_used)]
pub fn cmp(test_name: &'static str) {
    let mut file_name = PathBuf::from(test_name);

    file_name.push("target.rs");
    let target = read(&file_name).unwrap();
    let target = String::from_utf8_lossy(&target);

    file_name.set_file_name("source.rs");
    let cmd = Command::new(env!("CARGO_BIN_EXE_yew-fmt"))
        .args(["--emit", "stdout"])
        .arg(&file_name)
        .stdin(Stdio::null())
        .output()
        .expect("yew-fmt should be invoked");
    assert!(
        cmd.status.success(),
        "`yew-fmt --emit stdout {:?}` finished with a non-zero exit code\noutput:\n{}",
        &file_name,
        String::from_utf8_lossy(&cmd.stderr)
    );
    let source = String::from_utf8_lossy(
        &cmd.stdout[cmd.stdout.iter().position(|&x| x == b'\n').map_or(0, |x| x + 2)..],
    );
    assert!(
        source == target,
        "source and target differ:\n{}",
        diffy::create_patch(&target, &source)
    );
}
