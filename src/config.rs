use std::{fs::read_to_string, path::Path, io, env::current_dir, num::ParseIntError};
use dirs::{home_dir, config_dir};
use serde::Deserialize;
use anyhow::{Result, Context, anyhow, bail};
use toml::{map::Map, Value};
use crate::utils::StrExt;

#[derive(Clone)]
pub struct Config {
    pub tab_spaces: usize,
    pub yew: YewConfig,
}

impl Default for Config {
    fn default() -> Self {
        Self { tab_spaces: 4, yew: YewConfig::default() }
    }
}

#[derive(Clone)]
pub struct YewConfig {
    pub html_width: usize,
    pub unwrap_literal_prop_values: bool,
    pub use_prop_init_shorthand: bool,
    pub unknown: Map<String, Value>,
}

impl Default for YewConfig {
    fn default() -> Self {
        Self {
            html_width: 100,
            unwrap_literal_prop_values: true,
            use_prop_init_shorthand: false,
            unknown: Map::new(),
        }
    }
}

#[derive(Deserialize)]
struct RawConfig {
    tab_spaces: Option<usize>,
    max_width: Option<usize>,
    use_field_init_shorthand: Option<bool>,
    #[serde(default)]
    yew: RawConfigYew
}

#[derive(Deserialize, Default)]
struct RawConfigYew {
    html_width: Option<usize>,
    unwrap_literal_prop_values: Option<bool>,
    use_prop_init_shorthand: Option<bool>,
    #[serde(flatten)]
    unknown: Map<String, Value>,
}

fn parse_usize(src: &str) -> Result<usize, ParseIntError> {
    let (base, src) = match src.try_split_at(2) {
        Some(("0x", rest)) => (0x10, rest),
        Some(("0b", rest)) => (0b10, rest),
        Some(("0o", rest)) => (0o10, rest),
        _  => (10, src),
    };
    usize::from_str_radix(src, base)
}

fn parse_bool(src: &str) -> Result<bool> {
    match src {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => bail!("expected `true` or `false`, instead got `{src}`")
    }
}

impl Config {
    pub fn parse<'a>(
        src: &str,
        additional: impl IntoIterator<Item = &'a (impl AsRef<str> + 'a, impl AsRef<str> + 'a)>,
    ) -> Result<Self> {
        // TODO: rewrite with `concat_idents!(parser_, $ty)` once stabilised
        macro_rules! parser {
            [usize] => {parse_usize};
            [bool] => {parse_bool};
        }

        macro_rules! field_kind {
            [usize] => {"an integer"};
            [bool] => {"a boolean"};
        }

        macro_rules! parse_field {
            ($k:expr, $v:expr, $cfg:ident . { $($($name:ident).+ : $ty:ident),+ }) => {{
                let v = $v;
                match $k {
                    $(
                        stringify!($($name).+) => {
                            let err_msg = concat!(
                                "received a `",
                                stringify!($($name).+),
                                "` value that's not ",
                                field_kind!($ty)
                            );
                            $cfg.$($name).+ = Some(parser![$ty](v).context(err_msg)?);
                        }
                    )+
                    k => if let Some(k) = k.strip_prefix("yew.") {
                        $cfg.yew.unknown.insert(k.to_owned(), Value::Boolean(false));
                    }
                }
            }};
        }

        let mut raw: RawConfig = toml::from_str(src)?;
        for (key, value) in additional {
            parse_field!(key.as_ref(), value.as_ref(), raw.{
                tab_spaces: usize,
                max_width: usize,
                use_field_init_shorthand: bool,
                yew.html_width: usize,
                yew.unwrap_literal_prop_values: bool,
                yew.use_prop_init_shorthand: bool
            });
        }

        Ok(Self {
            tab_spaces: raw.tab_spaces.unwrap_or(4),
            yew: YewConfig {
                html_width: raw.yew.html_width
                    .or(raw.max_width)
                    .unwrap_or(100),
                unwrap_literal_prop_values: raw.yew.unwrap_literal_prop_values
                    .unwrap_or(true),
                use_prop_init_shorthand: raw.yew.use_prop_init_shorthand
                    .or(raw.use_field_init_shorthand)
                    .unwrap_or(false),
                unknown: raw.yew.unknown
            }
        })
    }

    pub fn fetch<'a>(
        path: Option<&Path>,
        additional: impl IntoIterator<Item = &'a (impl AsRef<str> + 'a, impl AsRef<str> + 'a)>
    ) -> Result<Self> {
        macro_rules! return_parsed_if_file_exists {
            ($path:expr) => {{
                let path: &Path = $path;
                match read_to_string(path) {
                    Ok(src) => return Self::parse(&src, additional),
                    Err(err) => if err.kind() != io::ErrorKind::NotFound {
                        return Err(anyhow!(err).context(format!("failed to read {path:?}")))
                    }
                }
            }};
        }

        if let Some(path) = path {
            let mut path = path.canonicalize()?;
            loop {
                path.push("rustfmt.toml");
                return_parsed_if_file_exists!(&path);
                path.set_file_name(".rustfmt.toml");
                return_parsed_if_file_exists!(&path);
                path.pop();
                if !path.pop() { break }
            }
        }

        let mut aqui = current_dir()?;
        loop {
            aqui.push("rustfmt.toml");
            return_parsed_if_file_exists!(&aqui);
            aqui.set_file_name(".rustfmt.toml");
            return_parsed_if_file_exists!(&aqui);
            aqui.pop();
            if !aqui.pop() { break }
        }

        let mut home = home_dir().context("failed to get the user's home directory")?;
        home.push("rustfmt.toml");
        return_parsed_if_file_exists!(&home);
        home.set_file_name(".rustfmt.toml");
        return_parsed_if_file_exists!(&home);

        let mut global = config_dir().context("failed to get the user's config directory")?;
        global.push("rustfmt.toml");
        return_parsed_if_file_exists!(&global);
        global.set_file_name(".rustfmt.toml");
        return_parsed_if_file_exists!(&global);

        Self::parse("", additional)
    }
}
