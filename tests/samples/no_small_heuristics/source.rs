// config: yew.use_small_heuristics="Off"

use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
<>
<div>
<code>
{ "Код!" }
</code>
</div>
if true { { "true" } } else {{"false"}}
</>
    }
}
