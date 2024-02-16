use crate::utils::{Result, StrExt};
use anyhow::{anyhow, bail, Context};
use dirs::{config_dir, home_dir};
use serde::{Deserialize, Deserializer};
use std::{
    collections::HashMap, env::current_dir, fs::read_to_string, io, num::ParseIntError, path::Path,
};

#[derive(Clone)]
pub struct Config {
    pub tab_spaces: usize,
    pub yew: YewConfig,
}

#[derive(Clone)]
pub struct YewConfig {
    pub html_width: usize,
    pub use_small_heuristics: UseSmallHeuristics,
    pub unwrap_literal_prop_values: bool,
    pub use_prop_init_shorthand: bool,
    pub self_close_elements: bool,
    pub ext: bool,
    pub unknown: HashMap<String, Unknown>,
}

#[derive(Deserialize)]
struct RawConfig {
    tab_spaces: Option<usize>,
    max_width: Option<usize>,
    use_field_init_shorthand: Option<bool>,
    use_small_heuristics: Option<UseSmallHeuristics>,
    #[serde(default)]
    yew: RawConfigYew,
}

#[derive(Deserialize, Default)]
struct RawConfigYew {
    html_width: Option<usize>,
    unwrap_literal_prop_values: Option<bool>,
    use_small_heuristics: Option<UseSmallHeuristics>,
    use_prop_init_shorthand: Option<bool>,
    self_close_elements: Option<bool>,
    ext: Option<bool>,
    #[serde(flatten)]
    unknown: HashMap<String, Unknown>,
}

/// exists to avoid any handling of the values of unknown keys
#[derive(Clone, Copy)]
pub struct Unknown;

impl<'de> Deserialize<'de> for Unknown {
    fn deserialize<D: Deserializer<'de>>(_: D) -> Result<Self, D::Error> {
        Ok(Self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum UseSmallHeuristics {
    Off,
    Default,
    Max,
}

fn parse_usize(src: &str) -> Result<usize, ParseIntError> {
    let (base, src) = match src.try_split_at(2) {
        Some(("0x", rest)) => (0x10, rest),
        Some(("0b", rest)) => (0b10, rest),
        Some(("0o", rest)) => (0o10, rest),
        _ => (10, src),
    };
    usize::from_str_radix(src, base)
}

fn parse_bool(src: &str) -> Result<bool> {
    Ok(match src {
        "true" => true,
        "false" => false,
        _ => bail!("expected `true` or `false`, instead got `{src}`"),
    })
}

fn parse_use_small_heuristics(src: &str) -> Result<UseSmallHeuristics> {
    Ok(match src {
        "\"Off\"" => UseSmallHeuristics::Off,
        "\"Default\"" => UseSmallHeuristics::Default,
        "\"Max\"" => UseSmallHeuristics::Max,
        _ => bail!(r#"expected `"Off"`, `"Default"` or `"Max"`, instead got `{src}`"#),
    })
}

impl Config {
    #[rustfmt::skip]
    pub fn parse<'ext>(
        src: &str,
        ext: impl IntoIterator<Item = &'ext (impl AsRef<str> + 'ext, impl AsRef<str> + 'ext)>,
    ) -> Result<Self> {
        // TODO: rewrite with `concat_idents!(parser_, $ty)` once stabilised
        macro_rules! parser {
            [usize] => {parse_usize};
            [bool] => {parse_bool};
            [UseSmallHeuristics] => {parse_use_small_heuristics};
        }

        macro_rules! field_kind {
            [usize] => {"an integer"};
            [bool] => {"a boolean"};
            [UseSmallHeuristics] => {"`use_small_heuristics` enum "};
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
                        $cfg.yew.unknown.insert(k.to_owned(), Unknown);
                    }
                }
            }};
        }

        let mut raw: RawConfig = basic_toml::from_str(src)?;
        for (key, value) in ext {
            parse_field!(key.as_ref(), value.as_ref(), raw.{
                tab_spaces: usize,
                max_width: usize,
                use_field_init_shorthand: bool,
                use_small_heuristics: UseSmallHeuristics,
                yew.html_width: usize,
                yew.use_small_heuristics: UseSmallHeuristics,
                yew.unwrap_literal_prop_values: bool,
                yew.use_prop_init_shorthand: bool,
                yew.self_close_elements: bool,
                yew.ext: bool
            });
        }

        Ok(Self {
            tab_spaces: raw.tab_spaces
                .unwrap_or(4),
            yew: YewConfig {
                html_width: raw.yew.html_width
                    .or(raw.max_width)
                    .unwrap_or(100),
                use_small_heuristics: raw.yew.use_small_heuristics
                    .or(raw.use_small_heuristics)
                    .unwrap_or(UseSmallHeuristics::Default),
                unwrap_literal_prop_values: raw.yew.unwrap_literal_prop_values
                    .unwrap_or(true),
                use_prop_init_shorthand: raw.yew.use_prop_init_shorthand
                    .or(raw.use_field_init_shorthand)
                    .unwrap_or(false),
                self_close_elements: raw.yew.self_close_elements
                    .unwrap_or(true),
                ext: raw.yew.ext
                    .unwrap_or(false),
                unknown: raw.yew.unknown,
            },
        })
    }

    pub fn fetch<'add>(
        path: Option<&Path>,
        additional: impl IntoIterator<Item = &'add (impl AsRef<str> + 'add, impl AsRef<str> + 'add)>,
    ) -> Result<Self> {
        macro_rules! return_parsed_if_file_exists {
            ($path:expr) => {{
                let path: &Path = $path;
                match read_to_string(path) {
                    Ok(src) => return Self::parse(&src, additional),
                    Err(err) => {
                        if err.kind() != io::ErrorKind::NotFound {
                            return Err(anyhow!(err).context(format!("failed to read {path:?}")));
                        }
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
                if !path.pop() {
                    break;
                }
            }
        }

        let mut aqui = current_dir()?;
        loop {
            aqui.push("rustfmt.toml");
            return_parsed_if_file_exists!(&aqui);
            aqui.set_file_name(".rustfmt.toml");
            return_parsed_if_file_exists!(&aqui);
            aqui.pop();
            if !aqui.pop() {
                break;
            }
        }

        let mut home = home_dir().context("failed to get the user's home directory")?;
        home.push("rustfmt.toml");
        return_parsed_if_file_exists!(&home);
        home.set_file_name(".rustfmt.toml");
        return_parsed_if_file_exists!(&home);

        let mut global = config_dir().context("failed to get the user's config directory")?;
        global.push("rustfmt");
        global.push("rustfmt.toml");
        return_parsed_if_file_exists!(&global);
        global.set_file_name(".rustfmt.toml");
        return_parsed_if_file_exists!(&global);

        Self::parse("", additional)
    }
}
