# yew-fmt
## Format HTML-enhanced Rust like normal Rust
`yew-fmt` is a drop-in replacement for [`rustfmt`](https://github.com/rust-lang/rustfmt), which means that, on top of all of its formatting,
it applies formatting to the HTML in [`yew::html!`](https://docs.rs/yew/latest/yew/macro.html.html) and [`yew::html_nested!`](https://docs.rs/yew/latest/yew/macro.html_nested.html).

## Install `yew-fmt`
Installing it is as easy as
```console
cargo install yew-fmt
```

## Configure `yew-fmt`
In the fashion of being a drop-in replacement, it inherits methods of configuration
from `rustfmt`, which is explained [here](https://rust-lang.github.io/rustfmt/),
and all of configuration options for `rustfmt` are also supported by `yew-fmt`.
On top of those, `yew-fmt` provides the following options of its own:

### `html_width`
Maximum width of an HTML node before falling back to vertical formatting.
- Default value: inherited from [`max_width`](https://rust-lang.github.io/rustfmt#max_width)
- Possible values: any positive integer

## Use `yew-fmt` in your project
After installing it, it can be used from the command line just like `rustfmt`.
To verify this, run the following command:
```console
yew-fmt --version
```

However, because it's a drop-in replacement for `rustfmt`, it can replace the latter to be
the backend for the `cargo fmt` command. To make it the default formatter on your machine,
export an environment variable `RUSTFMT` and set it to `yew-fmt`, on Linux/MacOS it can be done
with the following command:

```console
export RUSTFMT=yew-fmt
```

## Keep in mind, work is still in progress
As the project is very early on in development, not all CLI options of `rustfmt` are supported, yet the eventual target of the project is 100% compatibility, so all the ❌ in the table 
below will eventually become 🟩.

| CLI option | support |
|:--:|:--:|
| `--backup`               | 🟩 |
| `--check`                | 🟩 |
| `--color`                | 🟩 |
| `--config`               | 🟩 |
| `--config-path`          | 🟩 |
| `--edition`              | 🟩 |
| `--emit files`           | 🟩 |
| `--emit stdout`          | 🟩 |
| `--files-with-diff`      | 🟩 |
| `--print-config`         | ❌ |
| `--verbose`              | ❌ |
| `--quiet`                | 🟩 |

