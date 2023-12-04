use std::{ops::Deref, str::FromStr, fs::{File, write}, path::Path, io::{Read, Seek, Write, self, IoSlice}, fmt::Arguments};
use anyhow::{anyhow, Result, Context};
use codespan_reporting::term::termcolor::{HyperlinkSpec, ColorSpec, WriteColor};

pub trait StrExt {
    /// Returns the length of the last line of the string, or None if the string has only 1 line
    fn last_line_len(&self) -> Option<usize>;
    /// Unchecked version of `split_at`, caller must ensure that `self.is_char_boundary(mid)`
    unsafe fn split_at_unchecked(&self, mid: usize) -> (&str, &str);
    /// Non-panicking version of `split_at`
    fn try_split_at(&self, mid: usize) -> Option<(&str, &str)>;
}

impl StrExt for str {
    fn last_line_len(&self) -> Option<usize> {
        self.bytes().rev().enumerate().find_map(|(i, c)| (c == b'\n').then_some(i)) 
    }

    unsafe fn split_at_unchecked(&self, mid: usize) -> (&str, &str) {
        (self.get_unchecked(..mid), self.get_unchecked(mid..))
    }

    fn try_split_at(&self, mid: usize) -> Option<(&str, &str)> {
        self.is_char_boundary(mid).then(|| unsafe {
            // SAFETY: just checked that `mid` is on a char boundary.
            (self.get_unchecked(..mid), self.get_unchecked(mid..))
        })
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct KVPairs(Box<[(Box<str>, Box<str>)]>);

impl Deref for KVPairs {
    type Target = [(Box<str>, Box<str>)];
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl FromStr for KVPairs {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Self(Box::from([])))
        }
        s.split(',')
            .map(|p| p.split_once('=').map(|(k, v)| (k.into(), v.into())).ok_or(p))
            .collect::<Result<_, _>>()
            .map_err(|p| anyhow!("invalid key=val pair: `{p}`"))
            .map(Self)
    }
}

/// stores a flag that signals whether anything was written to the stream
pub struct Flagged<W> {
    pub inner: W,
    pub written: bool
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
    pub fn new(inner: W) -> Self { Self { inner, written: false } }
}

/// like `std::fs::write`, but will also create a `.bk` file
pub fn write_with_backup(filename: &str, new_text: &[u8]) -> Result<()> {
    let mut file = File::options().read(true).write(true).open(filename)
        .context("failed to open the file")?;
    let mut old_text = vec![];
    file.read_to_end(&mut old_text).context("failed to read the file")?;
    Ok(if &old_text[..] != new_text {
        let backup = Path::new(filename).with_extension("bk");
        write(&backup, old_text)
            .with_context(||
                format!("failed to create a backup file {:?}", backup.as_os_str()))?;
        file.rewind().context("failed to rewind the file handle")?;
        file.set_len(0).context("failed to clear the file")?;
        file.write_all(new_text).context("failed to write new data to the file")?;
    })
}

/// like `fs::read`, but allows for reusing allocations
pub fn read_into(file: impl AsRef<Path>, dst: &mut Vec<u8>) -> io::Result<()> {
    dst.clear();
    File::open(file)?.read_to_end(dst).map(drop)
}
