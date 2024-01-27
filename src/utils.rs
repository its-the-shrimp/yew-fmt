use anyhow::{anyhow, Context, Result};
use std::{
    fs::{write, File},
    io::{self, Read, Seek, Write},
    ops::Deref,
    path::Path,
    str::FromStr,
};

pub trait StrExt {
    /// Returns the length of the last line of the string, or `None` if the string is 1 line.
    fn last_line_len(&self) -> Option<usize>;
    /// Unchecked version of `split_at`, caller must ensure that `self.is_char_boundary(mid)`
    unsafe fn split_at_unchecked(&self, mid: usize) -> (&str, &str);
    /// Non-panicking version of `split_at`
    fn try_split_at(&self, mid: usize) -> Option<(&str, &str)>;
}

impl StrExt for str {
    fn last_line_len(&self) -> Option<usize> {
        self.bytes()
            .rev()
            .enumerate()
            .find_map(|(i, c)| (c == b'\n').then_some(i))
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

pub trait BoolExt {
    fn on_true(self, f: impl FnOnce()) -> Self;
}

impl BoolExt for bool {
    fn on_true(self, f: impl FnOnce()) -> Self {
        if self {
            f();
            true
        } else {
            false
        }
    }
}

pub trait OptionExt<T> {
    fn choose<U>(&self, on_true: U, on_false: U) -> U;
}

impl<T> OptionExt<T> for Option<T> {
    fn choose<U>(&self, on_true: U, on_false: U) -> U {
        if self.is_some() {
            on_true
        } else {
            on_false
        }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct KVPairs(Box<[(Box<str>, Box<str>)]>);

impl Deref for KVPairs {
    type Target = [(Box<str>, Box<str>)];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromStr for KVPairs {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Self(Box::from([])));
        }
        s.split(',')
            .map(|p| {
                p.split_once('=')
                    .map(|(k, v)| (k.into(), v.into()))
                    .ok_or(p)
            })
            .collect::<Result<_, _>>()
            .map_err(|p| anyhow!("invalid key=val pair: `{p}`"))
            .map(Self)
    }
}

pub struct WithPrevMut<'slice, T> {
    inner: &'slice mut [T],
    index: usize,
}

impl<'slice, T> WithPrevMut<'slice, T> {
    pub fn next(&mut self) -> Option<(&mut T, &mut [T])> {
        // Safety: if the slice is exhausted, the function will always return before reaching
        // `.unwrap_unchecked()`
        let res = self
            .inner
            .get_mut(..=self.index)
            .map(|x| unsafe { x.split_last_mut().unwrap_unchecked() });
        self.index += 1;
        res
    }
}

pub trait SliceExt<T> {
    fn iter_with_prev_mut(&mut self) -> WithPrevMut<'_, T>;
}

impl<T> SliceExt<T> for [T] {
    fn iter_with_prev_mut(&mut self) -> WithPrevMut<'_, T> {
        WithPrevMut {
            inner: self,
            index: 0,
        }
    }
}

/// like `std::fs::write`, but will also create a `.bk` file
pub fn write_with_backup(filename: &str, new_text: impl AsRef<[u8]>) -> Result<()> {
    let new_text = new_text.as_ref();
    let mut file = File::options()
        .read(true)
        .write(true)
        .open(filename)
        .context("failed to open the file")?;
    let mut old_text = vec![];
    file.read_to_end(&mut old_text)
        .context("failed to read the file")?;
    Ok(if &*old_text != new_text {
        let backup = Path::new(filename).with_extension("bk");
        write(&backup, old_text)
            .with_context(|| format!("failed to create a backup file {:?}", backup.as_os_str()))?;
        file.rewind().context("failed to rewind the file handle")?;
        file.set_len(0).context("failed to clear the file")?;
        file.write_all(new_text)
            .context("failed to write new data to the file")?;
    })
}

/// like `fs::read`, but allows for reusing allocations
pub fn read_into(file: impl AsRef<Path>, dst: &mut Vec<u8>) -> io::Result<()> {
    dst.clear();
    File::open(file)?.read_to_end(dst).map(drop)
}

/*#[macro_export]
macro_rules! bindings {
    ($done:tt $(,)*) => { $done };
    ($prev:tt, $fname:ident: $($next:tt)*) => {
        $crate::bindings!($prev, $($next)*)
    };
    ($prev:tt, $name:ident :: $(, $($next:tt)*)?) => {
        $crate::bindings!($prev, $($($next)*)?)
    };
    ($prev:tt, $literal:literal $(,$($next:tt)*)?) => {
        $crate::bindings!($prev, $($($next)*)?)
    };
    ($prev:tt, $($tuple:ident)? ($($p:tt)*) $(, $($next:tt)*)?) => {
        $crate::bindings!($prev, $($p)* $(, $($next)*)?)
    };
    ($prev:tt, $($tuple:ident)? {$($p:tt)*} $(, $($next:tt)*)?) => {
        $crate::bindings!($prev, $($p)* $(, $($next)*)?)
    };
    ($prev:tt, <$($_arg:tt)*> $(, $($next:tt)*)?) => {
        $crate::bindings!($prev, $($($next)*)?)
    };
    (($($prev:ident),*), $id:ident $(, $($next:tt)*)?) => {
        $crate::bindings!(($($prev,)* $id), $($($next)*)?)
    };
}*/

#[macro_export]
macro_rules! map {
    ($e:expr, $p:pat => $($binding:ident),*) => {
        match $e { $p => Some(($($binding),*)), _ => None }
    };
}
