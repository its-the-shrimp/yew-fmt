use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <>
            if smth {
                { "Это" }
            } else {
                { "То" }
            }
            <Smth ..props />
        </>
    }
}
