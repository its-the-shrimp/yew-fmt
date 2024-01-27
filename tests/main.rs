//! Auto-generated from build.rs

mod common;
use common::cmp;

#[test]
fn issue_10() {
    cmp("tests/samples/issue_10/source.rs", "tests/samples/issue_10/target.rs")
}

#[test]
fn breaking_propagated() {
    cmp(
        "tests/samples/breaking_propagated/source.rs",
        "tests/samples/breaking_propagated/target.rs",
    )
}

#[test]
fn issue_2() {
    cmp("tests/samples/issue_2/source.rs", "tests/samples/issue_2/target.rs")
}

#[test]
fn issue_5() {
    cmp("tests/samples/issue_5/source.rs", "tests/samples/issue_5/target.rs")
}

#[test]
fn issue_3() {
    cmp("tests/samples/issue_3/source.rs", "tests/samples/issue_3/target.rs")
}

#[test]
fn issue_6() {
    cmp("tests/samples/issue_6/source.rs", "tests/samples/issue_6/target.rs")
}

#[test]
fn issue_1() {
    cmp("tests/samples/issue_1/source.rs", "tests/samples/issue_1/target.rs")
}

#[test]
fn issue_8() {
    cmp("tests/samples/issue_8/source.rs", "tests/samples/issue_8/target.rs")
}

#[test]
fn issue_9() {
    cmp("tests/samples/issue_9/source.rs", "tests/samples/issue_9/target.rs")
}

#[test]
fn issue_7() {
    cmp("tests/samples/issue_7/source.rs", "tests/samples/issue_7/target.rs")
}
