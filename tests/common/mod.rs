use std::fs::read;
use std::process::{Command, Stdio};

#[allow(clippy::unwrap_used, clippy::expect_used)]
pub fn cmp(source_file: &'static str, target_file: &'static str) {
    let target = read(target_file).unwrap();
    let target = String::from_utf8_lossy(&target);
    let cmd = Command::new(env!("CARGO_BIN_EXE_yew-fmt"))
        .args(["--emit", "stdout", source_file])
        .stdin(Stdio::null())
        .output()
        .expect("yew-fmt should be invoked");
    assert!(
        cmd.status.success(),
        "`yew-fmt --emit stdout {:?}` finished with a non-zero exit code\noutput:\n{}",
        source_file,
        String::from_utf8_lossy(&cmd.stderr)
    );
    let source = String::from_utf8_lossy(
        &cmd.stdout[cmd
            .stdout
            .iter()
            .position(|&x| x == b'\n')
            .map_or(0, |x| x + 2)..],
    );
    if source != target {
        panic!(
            "source and target differ:\n{}",
            diffy::create_patch(&target, &source)
        );
    }
}
