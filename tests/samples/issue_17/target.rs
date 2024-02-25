use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    tml! {
        <div
            class={{
                let x = "foo";
                x
            }}
        />
    }
}
