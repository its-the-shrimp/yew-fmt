#![allow(clippy::unit_arg)]

mod formatter;
mod html;

use std::{
    process::{ExitCode, Command, Stdio},
    collections::HashMap,
    str::from_utf8,
    fmt::{Display, self, Arguments},
    fs::{OpenOptions, self, File},
    io::{Write, self, IoSlice, Read, Seek},
    path::Path,
};
use anyhow::{Context, Result};
use clap::{Parser, ValueEnum, ColorChoice as ColorWhen};
use codespan_reporting::term::termcolor::{ColorSpec, Color, ColorChoice, WriteColor,
    BufferedStandardStream, HyperlinkSpec};
use diffy::{create_patch, Line};
use formatter::Formatter;

/// stores a flag that signals whether anything was written to the stream
struct Flagged<W> {
    inner: W,
    written: bool
}

impl<W: Write> Write for Flagged<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.written = true;
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> { self.inner.flush() }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        self.written = true;
        self.inner.write_vectored(bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.written = true;
        self.inner.write_all(buf)
    }

    fn write_fmt(&mut self, fmt: Arguments<'_>) -> io::Result<()> {
        self.written = true;
        self.inner.write_fmt(fmt)
    }

    fn by_ref(&mut self) -> &mut Self { self }
}

impl<W: WriteColor> WriteColor for Flagged<W> {
    fn supports_color(&self) -> bool { self.inner.supports_color() }

    fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        self.written = true;
        self.inner.set_color(spec)
    }

    fn reset(&mut self) -> io::Result<()> {
        self.written = true;
        self.inner.reset()
    }

    fn is_synchronous(&self) -> bool { self.inner.is_synchronous() }

    fn set_hyperlink(&mut self, link: &HyperlinkSpec) -> io::Result<()> {
        self.written = true;
        self.inner.set_hyperlink(link)
    }

    fn supports_hyperlinks(&self) -> bool { self.inner.supports_hyperlinks() }
}

impl<W> Flagged<W> {
    fn new(inner: W) -> Self { Self { inner, written: false } }
}

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

/// like `fs::read`, but allows for reusing allocations
fn read_into(file: &str, dst: &mut Vec<u8>) -> io::Result<()> {
    File::open(file)?.read_to_end(dst).map(drop)
}

// `buf` must be passed into the function empty and is guaranteed to be empty after it
fn print_diff_for_file(
    buf: &mut Vec<u8>,
    out: &mut impl WriteColor,
    file: &str,
    new_text: &str,
) -> Result<()> {
    read_into(file, buf).context("failed to read contents of the source file")?;
    let src = from_utf8(buf).context("the source file is not UTF-8")?;
    let patch = create_patch(src, new_text);

    let mut color_spec = ColorSpec::new();
    Ok(for hunk in patch.hunks() {
        writeln!(out, "Diff in {file} at line {}:", hunk.old_range().start())
            .context("failed to write a diff header")?;
        for line in hunk.lines() {
            let (prefix, color, line) = match *line {
                Line::Context(line) => (' ', None, line),
                Line::Delete(line) => ('-', Some(Color::Red), line),
                Line::Insert(line) => ('+', Some(Color::Green), line),
            };
            color_spec.set_fg(color);
            out.set_color(&color_spec).context("failed to change diff buffer's color")?;
            write!(out, "{prefix}{line}")
                .context("failed to write a diff line")?;
            if !line.ends_with('\n') {
                writeln!(out).context("failed to put a newline")?;
            }
        }
        out.reset().context("failed to reset diff buffer's color")?;
    })
}

/// like `std::fs::write`, but will also create a `.bk` file
fn write_with_backup(filename: &str, new_text: &[u8]) -> Result<()> {
    let mut file = OpenOptions::new().read(true).write(true).open(filename)
        .context("failed to open the file")?;
    let mut old_text = vec![];
    file.read_to_end(&mut old_text).context("failed to read the file")?;
    Ok(if &old_text[..] != new_text {
        let backup = Path::new(filename).with_extension("bk");
        fs::write(&backup, old_text)
            .with_context(||
                format!("failed to create a backup file {:?}", backup.as_os_str()))?;
        file.rewind().context("failed to rewind the file handle")?;
        file.set_len(0).context("failed to clear the file")?;
        file.write_all(new_text).context("failed to write new data to the file")?;
    })
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
#[command(name = "yew-fmt", author, version, about)]
struct Cli {
    /// Backup any modified files
    #[arg(long, next_line_help = true, requires = "files", conflicts_with = "check")]
    backup: bool,
    /// Use colored output (if supported)
    #[arg(long, next_line_help = true, default_value_t, value_name = "when")]
    color: ColorWhen,
    /// Run in 'check' mode. Exits with 0 if input is formatted correctly. Exits with 1 and prints
    /// a diff if formatting is required.
    #[arg(long, next_line_help = true, requires = "files")]
    check: bool,
    /// Rust edition to use
    #[arg(long, next_line_help = true, value_name = "edition")]
    edition: Option<usize>,
    /// What data to emit and how
    #[arg(long, next_line_help = true, default_value_t, value_name = "what")]
    emit: EmitTarget,
    files: Vec<String>,
    /// Prints the names of mismatched files that were formatted.
    /// Prints the names of files that would be formatted when used with `--check` mode.
    #[arg(long, next_line_help = true, short = 'l')]
    files_with_diff: bool,
    /// Show less output
    #[arg(long, short, next_line_help = true)]
    quiet: bool,
}

pub fn main() -> anyhow::Result<ExitCode> {
    let args = Cli::parse();
    let color_choice = match args.color {
        ColorWhen::Auto => ColorChoice::Auto,
        ColorWhen::Always => ColorChoice::Always,
        ColorWhen::Never => ColorChoice::Never,
    };
    let mut stdout = Flagged::new(BufferedStandardStream::stdout(color_choice));
    let mut stderr = BufferedStandardStream::stderr(color_choice);
    // for reading files to get the source
    let mut src_buf = vec![];

    let mut rustfmt = Command::new("rustfmt");
    if let Some(edition) = args.edition {
        rustfmt.arg("--edition").arg(edition.to_string());
    }
    if args.quiet {
        rustfmt.arg("-q");
    }
    let rustfmt = rustfmt
        .args(["--emit", "stdout"])
        .arg("--color").arg(args.color.to_string())
        .args(&args.files)
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output().context("failed to run rustfmt")?;

    if !rustfmt.status.success() {
        return Ok(ExitCode::FAILURE);
    }

    let rustfmt_stdout = from_utf8(&rustfmt.stdout).context("failed to parse rustfmt's output")?;
    let mut formatter = Formatter::default();

    if args.files.is_empty() {
        let Some(out) = formatter.format("<stdin>", rustfmt_stdout)
            .context("failed to parse the input")?
            .emit_error(&mut stderr)
            .context("failed to print a syntax error in the input")?
            else { return Ok(ExitCode::FAILURE) };
        print!("{out}");
    }

    for (&file, &src) in parse_rustfmt_output(rustfmt_stdout, args.files.len()).iter() {
        let Some(out) = formatter.format(file, src)
            .with_context(|| format!("failed to parse {file:?}"))?
            .emit_error(&mut stderr)
            .with_context(|| format!("failed to print a syntax error in {file:?}"))?
            else { return Ok(ExitCode::FAILURE) };

        if args.check {
            if args.files_with_diff {
                read_into(file, &mut src_buf)
                    .with_context(|| format!("failed to read the contents of {file:?}"))?;
                if src_buf != out.as_bytes() {
                    println!("{file}");
                }
            } else {
                print_diff_for_file(&mut src_buf, &mut stdout, file, out)
                    .with_context(|| format!("failed to generate a diff for {file:?}"))?;
            }
            continue;
        }

        match args.emit {
            EmitTarget::Stdout =>
                println!("{file}:\n\n{out}"),
            EmitTarget::Files => {
                if args.backup {
                    write_with_backup(file, out.as_bytes())
                        .with_context(|| format!("failed to write to {file:?} with backup"))?;
                } else {
                    fs::write(file, out)
                        .with_context(|| format!("failed to write to {file:?}"))?;
                }
                if args.files_with_diff {
                    println!("{file}");
                }
            }
        }
    }

    stdout.flush().context("failed to flush stderr")
        .map(|_| if args.check && stdout.written {ExitCode::FAILURE} else {ExitCode::SUCCESS})
}
