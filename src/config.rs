use std::{fs::read_to_string, path::Path, io, env::current_dir, num::ParseIntError};
use dirs::{home_dir, config_dir};
use serde::Deserialize;
use anyhow::{Result, Context, anyhow};
use toml::{map::Map, Value};

#[derive(Clone)]
pub struct Config {
    pub tab_spaces: usize,
    pub yew: YewConfig
}

impl Default for Config {
    fn default() -> Self {
        Self { tab_spaces: 4, yew: YewConfig::default() }
    }
}

#[derive(Clone)]
pub struct YewConfig {
    pub html_width: usize,
    pub unknown: Map<String, Value>,
}

impl Default for YewConfig {
    fn default() -> Self {
        Self { html_width: 100, unknown: Map::new() }
    }
}

#[derive(Deserialize)]
struct RawConfig {
    tab_spaces: Option<usize>,
    max_width: Option<usize>,
    #[serde(default)]
    yew: RawConfigYew
}

#[derive(Deserialize, Default)]
struct RawConfigYew {
    html_width: Option<usize>,
    #[serde(flatten)]
    unknown: Map<String, Value>,
}

#[allow(clippy::from_str_radix_10)]
fn parse_int(src: &str) -> Result<usize, ParseIntError> {
    if let Some(src) = src.strip_prefix("0x") {
        usize::from_str_radix(src, 16)
    } else if let Some(src) = src.strip_prefix("0b") {
        usize::from_str_radix(src, 2)
    } else if let Some(src) = src.strip_prefix('0') {
        usize::from_str_radix(src, 8)
    } else {
        usize::from_str_radix(src, 10)
    }
}

impl Config {
    pub fn parse<'k, 'v>(
        src: &str,
        additional: impl IntoIterator<Item = (&'k str, &'v str)>,
    ) -> Result<Self> {
        let mut raw: RawConfig = toml::from_str(src)?;
        for (key, value) in additional {
            match key {
                "tab_spaces" => raw.tab_spaces = Some(parse_int(value)
                    .context("received a `tab_spaces` value that's not an integer")?),
                "max_width" => raw.max_width = Some(parse_int(value)
                    .context("received a `max_width` value that's not an integer")?),
                "yew.html_width" => raw.yew.html_width = Some(parse_int(value)
                    .context("received a `yew.html_width` value that's not an integer")?),
                _ => if let Some(key) = key.strip_prefix("yew.") {
                    raw.yew.unknown.insert(
                        key.to_owned(),
                        toml::from_str(value).unwrap_or(Value::Boolean(false))
                    );
                }
            }
        }

        Ok(Self {
            tab_spaces: raw.tab_spaces.unwrap_or(4),
            yew: YewConfig {
                html_width: raw.yew.html_width.or(raw.max_width).unwrap_or(100),
                unknown: raw.yew.unknown,
            }
        })
    }

    pub fn fetch<'k, 'v>(
        path: Option<&Path>,
        additional: impl IntoIterator<Item = (&'k str, &'v str)>,
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
