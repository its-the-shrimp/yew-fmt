// config: yew.use_small_heuristics="Max"
use yew::prelude::*;

#[function_component(App)]
pub fn app() -> Html {
    html! {
        if foo { <A long=".................................................." /> } else {
            <B long="..................................................................." />
        }
    }
}
