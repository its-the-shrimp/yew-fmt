use yew::prelude::*;

#[function_component(Application)]
fn app() -> Html {
    let loading = false;
    html! {
        if loading {
            { "Loading" }
        } else {
            { "Done" }
        }
    }
}
