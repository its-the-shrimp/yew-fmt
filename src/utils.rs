use anyhow::{anyhow, Context};
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::ToTokens;
use std::{
    fs::{write, File},
    io::{self, Read, Seek, Write},
    ops::Deref,
    path::Path,
    str::{CharIndices, FromStr},
};
use syn::{
    buffer::Cursor,
    ext::IdentExt,
    parse::{Parse, ParseStream},
};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;

pub trait StrExt {
    /// Returns the length of the last line of the string, or `None` if the string is 1 line.
    fn last_line_len(&self) -> Option<usize>;
    /// Unchecked version of `split_at`, caller must ensure that `self.is_char_boundary(mid)`
    unsafe fn split_at_unchecked(&self, mid: usize) -> (&str, &str);
    /// Like [`str::char_indices`], but treats a tab as being `tab_spaces` units long, reflecting
    /// its assumed visual representation.
    fn char_visual_offsets(&self, tab_spaces: usize) -> impl Iterator<Item = (usize, char)>;
}

impl StrExt for str {
    fn last_line_len(&self) -> Option<usize> {
        self.bytes().rev().enumerate().find_map(|(i, c)| (c == b'\n').then_some(i))
    }

    unsafe fn split_at_unchecked(&self, mid: usize) -> (&str, &str) {
        (self.get_unchecked(..mid), self.get_unchecked(mid..))
    }

    fn char_visual_offsets(&self, tab_spaces: usize) -> impl Iterator<Item = (usize, char)> {
        struct I<'src> {
            inner: CharIndices<'src>,
            /// 1 less than the supplied value
            tab_additional_length: usize,
            offset: usize,
        }

        impl Iterator for I<'_> {
            type Item = (usize, char);

            fn next(&mut self) -> Option<Self::Item> {
                let (off, ch) = self.inner.next()?;
                let off = off + self.offset;

                if ch == '\t' {
                    self.offset += self.tab_additional_length;
                }

                Some((off, ch))
            }
        }

        I { inner: self.char_indices(), tab_additional_length: tab_spaces - 1, offset: 0 }
    }
}

pub trait OptionExt<T> {
    fn choose<U>(&self, on_true: U, on_false: U) -> U;
    fn try_zip<U, E>(self, f: impl FnOnce() -> Result<U, E>) -> Result<Option<(T, U)>, E>;
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

    fn try_zip<U, E>(self, f: impl FnOnce() -> Result<U, E>) -> Result<Option<(T, U)>, E> {
        Ok(match self {
            Some(x) => Some((x, f()?)),
            None => None,
        })
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

impl<T> WithPrevMut<'_, T> {
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

/// Overrides `Ident`'s default `Parse` behaviour by accepting Rust keywords
pub struct AnyIdent(pub Ident);

impl Parse for AnyIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ident::parse_any(input).map(Self)
    }
}

impl ToTokens for AnyIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl Deref for AnyIdent {
    type Target = Ident;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
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
