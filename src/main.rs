#![allow(clippy::unit_arg)]

mod formatter;
mod html;

use std::{
    process::{ExitCode, Command, Stdio},
    collections::HashMap,
    str::from_utf8,
    fmt::{Display, self},
    fs::OpenOptions,
    io::Write,
};
use anyhow::Context;
use clap::{Parser, ValueEnum};
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, ValueEnum)]
enum EmitTarget {
    #[default]
    Files,
    Stdout,
}

impl Display for EmitTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Stdout => "stdout",
            Self::Files => "files",
        })
    }
}

#[derive(Parser)]
#[command(name = "yew-fmt", author, about)]
struct Cli {
    #[arg(
        long,
        default_value_t,
        value_name = "what",
        help = "What data to emit and how",
        next_line_help = true,
    )]
    emit: EmitTarget,
    #[arg(required = true)]
    files: Vec<String>,
}

pub fn main() -> anyhow::Result<ExitCode> {
    let Cli { emit, files } = Cli::parse();
    let rustfmt = Command::new("rustfmt")
        .args(["--emit", "stdout"])
        .args(&files)
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output().context("failed to run rustfmt")?;

    if !rustfmt.status.success() {
        return Ok(ExitCode::FAILURE);
    }

    let files = parse_rustfmt_output(
        from_utf8(&rustfmt.stdout).context("failed to parse rustfmt's output")?,
        files.len()
    );
    let mut formatter = Formatter::default();

    for (&file, &src) in files.iter() {
        let res = formatter.format(file, src)
            .with_context(|| format!("failed to parse {file:?}"))?;
        let Some(out) = res.emit_error()
            .with_context(|| format!("failed to print a syntax error in {file:?}"))?
            else { return Ok(ExitCode::FAILURE) };
        match emit {
            EmitTarget::Stdout =>
                println!("{file}:\n\n{out}"),
            EmitTarget::Files =>
                _ = OpenOptions::new().write(true).truncate(true).open(file)
                    .with_context(|| format!("failed to open {file:?} for writing"))?
                    .write(out.as_bytes())
                    .with_context(|| format!("failed to write to {file:?}"))?,
        }
    }
    Ok(ExitCode::SUCCESS)
}
