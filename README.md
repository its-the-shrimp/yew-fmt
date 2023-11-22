# yew-fmt
## Format HTML-enhanced Rust like normal Rust
`yew-fmt` is a drop-in replacement for [`rustfmt`](https://github.com/rust-lang/rustfmt), which means that, on top of all of its formatting,
it applies formatting to the HTML in [`yew::html!`](https://docs.rs/yew/latest/yew/macro.html.html) and [`yew::html_nested!`](https://docs.rs/yew/latest/yew/macro.html_nested.html).

## Install `yew-fmt`
Installing it as easy as 
```console
cargo install yew-fmt
```

## Use `yew-fmt` in your project
After installing it, it can be used from the command line just like `rustfmt`.
However, because it's a drop-in replacement for `rustfmt`, it can replace the latter to be
the backend for the `cargo fmt` command. To make it the default formatter for your project, add the following lines in your project's `.cargo/config.toml`:
```toml
[env]
RUSTFMT = "yew-fmt"
```
This will set the environment variable that `cargo` looks for when `cargo fmt` is invoked.

## Keep in mind, work is still in progress
As the project is very early on in development, not all CLI options of `rustfmt` are supported, yet the eventual target of the project is 100% compatibility, so all the ❌ in the table 
below will eventually become 🟩.

| CLI option | support |
|:--:|:--:|
| `--backup`               | ❌ |
| `--check`                | ❌ |
| `--color`                | ❌ |
| `--edition`              | ❌ |
| `--config-file`          | ❌ |
| `--emit files`           | 🟩 |
| `--emit json`            | ❌ |
| `--emit stdout`          | 🟩 |
| `--error-on-unformatted` | ❌ |
| `--file-lines`           | ❌ |
| `--files-with-diff`      | ❌ |
| `--verbose`              | ❌ |
| `--quiet`                | ❌ |
| `--config`               | ❌ |

