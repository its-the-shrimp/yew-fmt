use crate::{html::HtmlFlavor, utils::Result};
use anyhow::{anyhow, bail, Context};
use dirs::{config_dir, home_dir};
use serde::{de::IgnoredAny, Deserialize};
use std::{
    collections::HashMap, env::current_dir, fs::read_to_string, io, num::ParseIntError, path::Path,
};

#[derive(Clone)]
pub struct Config {
    pub hard_tabs: bool,
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
    pub format_css: bool,
    pub html_flavor: HtmlFlavor,
    pub unknown: HashMap<String, IgnoredAny>,
}

#[derive(Deserialize)]
struct RawConfig {
    max_width: Option<usize>,
    use_field_init_shorthand: Option<bool>,
    use_small_heuristics: Option<UseSmallHeuristics>,
    // The fields above are only needed as default values for the respective `yew.` values,
    // so they don't remain in the parsed `Config`
    hard_tabs: Option<bool>,
    tab_spaces: Option<usize>,
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
    format_css: Option<bool>,
    html_flavor: Option<HtmlFlavor>,
    #[serde(flatten)]
    unknown: HashMap<String, IgnoredAny>,
}

#[derive(Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum UseSmallHeuristics {
    Off,
    Default,
    Max,
}

fn parse_usize(src: &str) -> Result<usize, ParseIntError> {
    let (base, src) = match src.split_at_checked(2) {
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

fn parse_html_flavor(src: &str) -> Result<HtmlFlavor> {
    Ok(match src {
        "\"Base\"" => HtmlFlavor::Base,
        "\"Ext\"" => HtmlFlavor::Ext,
        _ => bail!(r#"expected `"Base"` or `"Ext"`, instead got `{src}`"#),
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
            [HtmlFlavor] => {parse_html_flavor};
        }

        macro_rules! field_kind {
            [usize] => {"an integer"};
            [bool] => {"a boolean"};
            [UseSmallHeuristics] => {"`use_small_heuristics` enum "};
            [HtmlFlavor] => {"`html_flavor` enum"};
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
                        $cfg.yew.unknown.insert(k.to_owned(), IgnoredAny);
                    }
                }
            }};
        }

        let mut raw: RawConfig = basic_toml::from_str(src)?;
        for (key, value) in ext {
            parse_field!(key.as_ref(), value.as_ref(), raw.{
                hard_tabs: bool,
                tab_spaces: usize,
                max_width: usize,
                use_field_init_shorthand: bool,
                use_small_heuristics: UseSmallHeuristics,
                yew.html_width: usize,
                yew.use_small_heuristics: UseSmallHeuristics,
                yew.unwrap_literal_prop_values: bool,
                yew.use_prop_init_shorthand: bool,
                yew.self_close_elements: bool,
                yew.format_css: bool,
                yew.html_flavor: HtmlFlavor
            });
        }

        Ok(Self {
            hard_tabs: raw.hard_tabs
                .unwrap_or(false),
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
                format_css: raw.yew.format_css
                    .unwrap_or(true),
                html_flavor: raw.yew.html_flavor
                    .unwrap_or(HtmlFlavor::Base),
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

    pub fn print_break(&self, out: &mut String, n_newlines: u8, mut indent: usize) {
        if n_newlines == 0 {
            return;
        }
        out.reserve(indent + 1);
        for _ in 0..n_newlines {
            out.push('\n')
        }

        if self.hard_tabs {
            let n_tabs = indent / self.tab_spaces;
            for _ in 0..n_tabs {
                out.push('\t');
            }
            indent %= self.tab_spaces;
        }

        for _ in 0..indent {
            out.push(' ');
        }
    }

    pub fn is_inline_css_attr(&self, element: &str, attr: &str) -> bool {
        match element {
            "a" | "abbr" | "article" | "aside" | "audio" | "b" | "blockquote" | "br" | "button"
            | "canvas" | "caption" | "cite" | "code" | "col" | "colgroup" | "details" | "div"
            | "dl" | "dt" | "dd" | "em" | "figcaption" | "figure" | "fieldset" | "footer"
            | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hr" | "i"
            | "iframe" | "img" | "input" | "label" | "legend" | "li" | "main" | "mark"
            | "meter" | "nav" | "ol" | "option" | "p" | "pre" | "progress" | "section"
            | "select" | "small" | "span" | "strong" | "sub" | "summary" | "sup" | "table"
            | "tbody" | "td" | "textarea" | "tfoot" | "th" | "thead" | "time" | "tr" | "u"
            | "ul" | "video" => attr == "style",

            _ => false,
        }
    }
}
