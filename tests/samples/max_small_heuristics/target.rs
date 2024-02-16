// config: yew.use_small_heuristics="Max",yew.ext=true

use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! { <><div><code>{ "Код!" }</code></div>if true { { "true" } } else { { "false" } }</> };
    html!(for i in 0..10 { { i } })
}
