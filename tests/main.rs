mod common;
use common::cmp;

#[test]
fn issue_2() {
    cmp(
        "tests/samples/issue_2/source.rs",
        "tests/samples/issue_2/target.rs",
    )
}

#[test]
fn issue_1() {
    cmp(
        "tests/samples/issue_1/source.rs",
        "tests/samples/issue_1/target.rs",
    )
}
