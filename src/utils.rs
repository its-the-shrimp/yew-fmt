use anyhow::{anyhow, Context};
use proc_macro2::{TokenStream, TokenTree};
use std::{
    fs::{write, File},
    io::{self, Read, Seek, Write},
    ops::Deref,
    path::Path,
    str::FromStr,
};
use syn::{
    buffer::Cursor,
    parse::{Parse, ParseBuffer, ParseStream, Parser},
    punctuated::Punctuated,
};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;

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

pub trait OptionExt<T> {
    fn choose<U>(&self, on_true: U, on_false: U) -> U;
    fn try_map_or<U, E>(self, default: U, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn choose<U>(&self, on_some: U, on_none: U) -> U {
        if self.is_some() {
            on_some
        } else {
            on_none
        }
    }

    fn try_map_or<U, E>(self, default: U, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E> {
        match self {
            Some(x) => f(x),
            None => Ok(default),
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
            .map(|p| p.split_once('=').map(|(k, v)| (k.into(), v.into())).ok_or(p))
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
        WithPrevMut { inner: self, index: 0 }
    }
}

pub struct TokenIter<'cursor>(pub Cursor<'cursor>);

impl Iterator for TokenIter<'_> {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, new_cursor) = self.0.token_tree()?;
        self.0 = new_cursor;
        Some(token)
    }
}

pub trait TokenTreeExt {
    fn is_ident(&self, ident: &str) -> bool;
}

impl TokenTreeExt for TokenTree {
    fn is_ident(&self, ident: &str) -> bool {
        matches!(self, TokenTree::Ident(i) if i == ident)
    }
}

pub trait ParseWithCtx: Sized {
    type Context;

    fn parse(input: ParseStream, ctx: Self::Context) -> syn::Result<Self>;
}

impl<T: ParseWithCtx> ParseWithCtx for Box<T> {
    type Context = T::Context;

    fn parse(input: ParseStream, ctx: Self::Context) -> syn::Result<Self> {
        T::parse(input, ctx).map(Box::new)
    }
}

pub trait ParseBufferExt {
    fn parse_with_ctx<T: ParseWithCtx>(&self, ctx: T::Context) -> syn::Result<T>;
}

impl<'buf> ParseBufferExt for ParseBuffer<'buf> {
    fn parse_with_ctx<T: ParseWithCtx>(&self, ctx: T::Context) -> syn::Result<T> {
        T::parse(self, ctx)
    }
}

pub trait PunctuatedExt<T>
where
    Self: Sized,
    T: ParseWithCtx,
    T::Context: Copy,
{
    fn parse_terminated_with_ctx(input: ParseStream, ctx: T::Context) -> syn::Result<Self>;
}

impl<T, P> PunctuatedExt<T> for Punctuated<T, P>
where
    T: ParseWithCtx,
    T::Context: Copy,
    P: Parse,
{
    fn parse_terminated_with_ctx(input: ParseStream, ctx: T::Context) -> syn::Result<Self> {
        let mut punctuated = Punctuated::new();

        loop {
            if input.is_empty() {
                break;
            }
            let value = input.parse_with_ctx(ctx)?;
            punctuated.push_value(value);
            if input.is_empty() {
                break;
            }
            let punct = input.parse()?;
            punctuated.push_punct(punct);
        }

        Ok(punctuated)
    }
}

pub fn parse2_with_ctx<T: ParseWithCtx>(stream: TokenStream, ctx: T::Context) -> syn::Result<T> {
    (|input: ParseStream| T::parse(input, ctx)).parse2(stream)
}

/// like `std::fs::write`, but will also create a `.bk` file
pub fn write_with_backup(filename: &str, new_text: impl AsRef<[u8]>) -> Result {
    let new_text = new_text.as_ref();
    let mut file =
        File::options().read(true).write(true).open(filename).context("failed to open the file")?;
    let mut old_text = vec![];
    file.read_to_end(&mut old_text).context("failed to read the file")?;
    if &*old_text != new_text {
        let backup = Path::new(filename).with_extension("bk");
        write(&backup, old_text)
            .with_context(|| format!("failed to create a backup file {:?}", backup.as_os_str()))?;
        file.rewind().context("failed to rewind the file handle")?;
        file.set_len(0).context("failed to clear the file")?;
        file.write_all(new_text).context("failed to write new data to the file")?;
    }
    Ok(())
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

pub fn default<T: Default>() -> T {
    T::default()
}
