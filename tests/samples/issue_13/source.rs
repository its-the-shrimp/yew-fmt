use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <SomeComponent attr_one=true>
            { "Very long text that will stop these three lines from being merged" }
        </SomeComponent>
    }
}
