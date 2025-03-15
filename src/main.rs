mod config;
mod css;
mod formatter;
mod html;
mod utils;

use anyhow::Context;
use clap::{ColorChoice as ColorWhen, Parser, ValueEnum};
use codespan_reporting::term::termcolor::{
    BufferWriter, Color, ColorChoice, ColorSpec, StandardStream, WriteColor,
};
use config::Config;
use diffy::{create_patch, Line};
use formatter::Formatter;
use std::{
    collections::HashMap,
    fs::write,
    io::Write,
    path::{Component, Path, PathBuf},
    process::{Command, ExitCode, Stdio},
    str::from_utf8,
};
use utils::{read_into, write_with_backup, KVPairs, Result};

fn parse_rustfmt_output(src: &str) -> Result<HashMap<&str, &str>> {
    fn path_like(src: &str) -> bool {
        let Some(src) = src.strip_suffix(':').filter(|x| !x.starts_with("//")) else {
            return false;
        };
        matches!(
            Path::new(src).components().next(),
            Some(Component::RootDir) | Some(Component::Prefix(_))
        )
    }

    let mut res = HashMap::with_capacity(0);
    let mut prev_entry: Option<&str> = None;
    for l in src.lines() {
        if path_like(l) {
            if let Some(name) = prev_entry.as_mut() {
                let start = name.as_ptr() as usize - src.as_ptr() as usize + name.len() + 3;
                let end = l.as_ptr() as usize - src.as_ptr() as usize;
                res.insert(*name, &src[start..end]);
                *name = &l[..l.len() - 1];
            } else {
                prev_entry = Some(&l[..l.len() - 1]);
            }
        }
    }
    if let Some(name) = prev_entry {
        let start = name.as_ptr() as usize - src.as_ptr() as usize + name.len() + 3;
        res.insert(name, &src[start..]);
    }
    Ok(res)
}

// `buf` must be passed into the function empty and is guaranteed to be empty after it
fn print_diff_for_file(
    buf: &mut Vec<u8>,
    out: &mut impl WriteColor,
    file: &str,
    new_text: &str,
) -> Result {
    read_into(file, buf).context("failed to read contents of the source file")?;
    let src = from_utf8(buf).context("the source file is not UTF-8")?;
    let patch = create_patch(src, new_text);

    let mut color_spec = ColorSpec::new();
    for hunk in patch.hunks() {
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
            write!(out, "{prefix}{line}").context("failed to write a diff line")?;
            if !line.ends_with('\n') {
                writeln!(out).context("failed to put a newline")?;
            }
        }
        out.reset().context("failed to reset diff buffer's color")?;
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum EmitTarget {
    Files,
    Stdout,
}

#[derive(Parser)]
#[command(name = "yew-fmt", author, version, about)]
struct Cli {
    /// Backup any modified files
    #[arg(long, next_line_help = true, requires = "files", conflicts_with = "check")]
    backup: bool,
    /// Run in 'check' mode. Exits with 0 if input is formatted correctly. Exits with 1 and prints
    /// a diff if formatting is required.
    #[arg(long, next_line_help = true, requires = "files")]
    check: bool,
    /// Use colored output (if supported)
    #[arg(long, next_line_help = true, default_value_t, value_name = "when")]
    color: ColorWhen,
    /// Set options from command line. These settings take priority over .rustfmt.toml
    #[arg(
        long,
        next_line_help = true,
        default_value = "",
        value_name = "key1=val1,key2=val2...",
        hide_default_value = true
    )]
    config: KVPairs,
    /// Recursively searches the given path for the rustfmt.toml config file.
    /// If not found reverts to the input file path
    #[arg(long, next_line_help = true, value_name = "path")]
    config_path: Option<PathBuf>,
    /// Rust edition to use
    #[arg(long, next_line_help = true, value_name = "edition")]
    edition: Option<usize>,
    /// What data to emit and how
    #[arg(long, next_line_help = true, default_value = "files", value_name = "what")]
    emit: EmitTarget,
    /// Prints the names of mismatched files that were formatted.
    /// Prints the names of files that would be formatted when used with `--check` mode.
    #[arg(long, next_line_help = true, short = 'l')]
    files_with_diff: bool,
    /// Show less output
    #[arg(long, short, next_line_help = true)]
    quiet: bool,

    files: Vec<PathBuf>,
}

pub fn main() -> anyhow::Result<ExitCode> {
    let args = Cli::parse();
    let color_choice = match args.color {
        ColorWhen::Auto => ColorChoice::Auto,
        ColorWhen::Always => ColorChoice::Always,
        ColorWhen::Never => ColorChoice::Never,
    };
    let actual_stdout = BufferWriter::stdout(color_choice);
    let mut stdout = actual_stdout.buffer();
    let mut stderr = StandardStream::stderr(color_choice);
    // for reading files to get the source
    let mut src_buf = vec![];

    let mut rustfmt = Command::new("rustfmt");
    rustfmt.arg("--color").arg(args.color.to_string());
    if !args.config.is_empty() {
        let no_yew_fields: String = args
            .config
            .iter()
            .filter(|(k, _)| !k.starts_with("yew."))
            .flat_map(|(k, v)| [",", k, "=", v])
            // TODO: replace with `intersperse` once stabilised
            .skip(1)
            .collect();
        if !no_yew_fields.is_empty() {
            rustfmt.arg("--config").arg(no_yew_fields);
        }
    }
    if let Some(config_path) = &args.config_path {
        rustfmt.arg("--config-path").arg(config_path);
    }
    if let Some(edition) = args.edition {
        rustfmt.arg("--edition").arg(edition.to_string());
    }
    rustfmt.args(["--emit", "stdout"]);
    if args.quiet {
        rustfmt.arg("-q");
    }
    let rustfmt = rustfmt
        .args(&args.files)
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context("failed to run rustfmt")?;

    from_utf8(&rustfmt.stderr)
        .context("failed to parse rustfmt's stderr")?
        .lines()
        .skip_while(|&l| l == "Warning: Unknown configuration option `yew`")
        .for_each(|l| eprintln!("{l}"));

    if !rustfmt.status.success() {
        return Ok(ExitCode::FAILURE);
    }

    let config = Config::fetch(args.config_path.as_deref(), &*args.config)
        .context("failed to fetch the config")?;
    for key in config.yew.unknown.keys() {
        eprintln!("Warning: Unknown configuration option `yew.{key}`");
    }
    let mut formatter = Formatter::new(config);
    let rustfmt_stdout = from_utf8(&rustfmt.stdout).context("failed to parse rustfmt's output")?;

    if args.files.is_empty() {
        let Some(out) = formatter
            .format("<stdin>", rustfmt_stdout)
            .context("failed to parse the input")?
            .emit_error(&mut stderr)
            .context("failed to print a syntax error in the input")?
        else {
            return Ok(ExitCode::FAILURE);
        };
        print!("{out}");
        return Ok(ExitCode::SUCCESS);
    }
    let rustfmt_output =
        parse_rustfmt_output(rustfmt_stdout).context("failed to parse rustfmt output")?;
    for (&file, &src) in &rustfmt_output {
        let Some(out) = formatter
            .format(file, src)
            .with_context(|| format!("failed to parse {file:?}"))?
            .emit_error(&mut stderr)
            .with_context(|| format!("failed to print a syntax error in {file:?}"))?
        else {
            return Ok(ExitCode::FAILURE);
        };

        if args.check {
            read_into(file, &mut src_buf)
                .with_context(|| format!("failed to read the contents of {file:?}"))?;
            if args.files_with_diff {
                if src_buf != out.as_bytes() {
                    write!(stdout, "{file}").context("failed to write a filename to stdout")?;
                }
            } else {
                print_diff_for_file(&mut src_buf, &mut stdout, file, out)
                    .with_context(|| format!("failed to generate a diff for {file:?}"))?;
            }
            continue;
        }

        match args.emit {
            EmitTarget::Stdout => print!("{file}:\n\n{out}"),
            EmitTarget::Files => {
                if args.backup {
                    write_with_backup(file, out)
                        .with_context(|| format!("failed to write to {file:?} with backup"))?;
                } else {
                    write(file, out).with_context(|| format!("failed to write to {file:?}"))?;
                }
                if args.files_with_diff {
                    println!("{file}");
                }
            }
        }
    }

    let stdout_written_to = !stdout.is_empty();
    actual_stdout.print(&stdout).context("failed to flush stdout")?;
    Ok(if stdout_written_to { ExitCode::FAILURE } else { ExitCode::SUCCESS })
}
