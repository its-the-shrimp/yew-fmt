# yew-fmt
## Format HTML-enhanced Rust like normal Rust
`yew-fmt` is a drop-in replacement for [`rustfmt`](https://github.com/rust-lang/rustfmt),
which means that, on top of all of its formatting,
it applies formatting to the HTML in [`yew::html!`](https://docs.rs/yew/latest/yew/macro.html.html)
and [`yew::html_nested!`](https://docs.rs/yew/latest/yew/macro.html_nested.html), as well
as formatting the inline CSS in the HTML elements.

## Install `yew-fmt`
Installing it is as easy as
```console
cargo install yew-fmt
```

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

## Configure `yew-fmt`
In the fashion of being a drop-in replacement, it inherits methods of configuration
from `rustfmt`, which is explained [here](https://rust-lang.github.io/rustfmt/),
and all of configuration options for `rustfmt` are also supported by `yew-fmt`.
On top of those, `yew-fmt` provides the following options of its own:

### `yew.html_width`
Maximum width of an HTML node before falling back to vertical formatting.
- Default value: inherited from [`max_width`](https://rust-lang.github.io/rustfmt#max_width)
- Possible values: any positive integer

### `yew.use_small_heuristics`
Along with `yew.html_width`, different kinds of nodes have different rules for when to be broken up. This option controls what set of rules to use.
- Default value: inherited from [`use_small_heuristics`](https://rust-lang.github.io/rustfmt#use_small_heuristics)
- Possible values: `"Default"`, `"Off"`, `"Max"`
#### `Default`
Elements are not broken up if they have no children or if all their children are block nodes, otherwise they're broken up.</br>
`if` nodes are always broken up
```html
<div>
    <code>{ "panic!(" }{ msg }{ ")" }</code>
</div>
if condition {
    <p>{ "Something else" }</p>
}
```
#### `Max`
Elements can only be broken up due to exceeding `yew.html_width`.</br>
`if` nodes will remain on 1 line unless they exceed `yew.html_width`
```html
<div><code>{ "panic!(" }{ msg }{ ")" }</code></div>
if condition { <p>{ "Something else" }</p> }
```
#### `Off`
Elements and `if` nodes are always broken up
```html
<div>
    <code>
        { "panic!(" }
        { msg }
        { ")" }
    </code>
</div>
if condition {
    <p>
        { "Something else" }
    </p>
}
```

### `yew.use_prop_init_shorthand`
Use prop initialiser shorthand if possible.
- Default value: inherited from
    [`use_field_init_shorthand`](https://rust-lang.github.io/rustfmt#use_field_init_shorthand)
- Possible values: `true`, `false`
#### `false`
```html
<div id={id} />
```
#### `true`
```html
<div {id} />
```

### `yew.unwrap_literal_prop_values`
Remove braces around prop initialisers if they consist of only a literal.
- Default value: `true`
- Possible values: `true`, `false`
#### `false`
```html
<div id={"foo"} class="bar" />
```
#### `true`
```html
<div id="foo" class="bar" />
```

### `yew.self_close_elements`
Make elements self-closed if they have no children.
- Default value: `true`
- Possible values: `true`, `false`
#### `false`
```html
<div id="foo"></div>
```
#### `true`
```html
<div id="foo" />
```

### `yew.format_css`
Format inline CSS in the `style` attribute of base HTML elements. 
- Default value: `true`
- Possible values: `true`, `false`
#### `false`
```html
<div style="text-decoration:none;       display: flex" />
```

#### `true`
```html
<div style="text-decoration: none; display: flex" />
```

### `yew.html_flavor`
By default `yew-fmt` formats the HTML using Yew's original syntax, however,
this feature allows for switching the HTML flavor used to accomodate e.g. community-made variations.
As of now, the only supported syntactic variation is that of
[`yew-html-ext`](https://github.com/schvv31n/yew-html-ext)
- Default value: `"Base"`
- Possible values: `"Base"`, `"Ext"`
#### `Base`
The default syntax specified by Yew itself.
#### `Ext`
The syntax of `yew-html-ext`.

## Keep in mind, work is still in progress
As the project is very early on in development, not all CLI options of `rustfmt` are supported,
yet the eventual target of the project is 100% compatibility, so all the ❌ in the table 
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

