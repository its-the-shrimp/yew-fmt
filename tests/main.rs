mod common;
use common::cmp;

#[test]
fn issue_1() {
    cmp(
        "tests/samples/issue_1/source.rs",
        "tests/samples/issue_1/target.rs",
    )
}
