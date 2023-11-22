#![allow(clippy::unit_arg)]

mod formatter;
mod html;

use std::{
    process::{ExitCode, Command, Stdio},
    collections::HashMap,
    env::args_os,
    str::from_utf8
};
use anyhow::Context;
use clap::Parser;
use formatter::Formatter;

fn parse_rustfmt_output<'a>(
    input: &'a str,
    n_files_hint: usize
) -> HashMap<&'a str, &'a str> {
    let mut res = HashMap::with_capacity(n_files_hint);
    let mut prev_entry: Option<&'a str> = None;
    for l in input.lines() {
        if l.starts_with('/') && l.ends_with(':') {
            if let Some(name) = prev_entry.as_mut() {
                let start = name.as_ptr() as usize - input.as_ptr() as usize + name.len() + 3;
                let end = l.as_ptr() as usize - input.as_ptr() as usize;
                unsafe {
                    res.insert(*name, input.get_unchecked(start .. end));
                    *name = l.get_unchecked(.. l.len() - 1);
                }
            } else {
                unsafe {
                    prev_entry = Some(l.get_unchecked(.. l.len() - 1));
                }
            }
        }
    }
    if let Some(name) = prev_entry {
        let start = name.as_ptr() as usize - input.as_ptr() as usize + name.len() + 3;
        unsafe {
            res.insert(name, input.get_unchecked(start .. ));
        }
    }
    res
}

#[derive(Parser)]
#[command(name = "yew-fmt", author, about)]
struct Cli {
    #[arg(required = true)]
    files: Vec<String>,
}

pub fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();

    let rustfmt = Command::new("rustfmt")
        .args(["--emit", "stdout"]).args(args_os().skip(1))
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output().context("failed to run rustfmt")?;

    if !rustfmt.status.success() {
        return Ok(ExitCode::FAILURE);
    }

    let files = parse_rustfmt_output(
        from_utf8(&rustfmt.stdout).context("failed to parse rustfmt's output")?,
        cli.files.len()
    );
    let mut formatter = Formatter::default();

    for (&file, &src) in files.iter() {
        let res = formatter.format(file, src)
            .with_context(|| format!("failed to parse {file:?}"))?;
        let Some(out) = res.emit_error()
            .with_context(|| format!("failed to print a syntax error in {file:?}"))?
            else { return Ok(ExitCode::FAILURE) };
        println!("{file}:\n\n{out}");
    }
    Ok(ExitCode::SUCCESS)
}
