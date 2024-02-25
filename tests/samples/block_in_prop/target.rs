use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html!(<div id={let id = "idk"; id} />);
    html!(
        <div
            id={let id = "one line
                             another line"; id}
        />
    )
}
