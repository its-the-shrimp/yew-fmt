use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    let loading=true;

    html! {
        if loading  {
            { "Loading" }
        } else if false {
            { "This shouldn't happen" }
        } else {
            { "Done" }
        }
    }
}
