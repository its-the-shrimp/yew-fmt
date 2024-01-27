use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <Button
            name="Play"
            onclick={emitter.reform(|_| AppEvent::PreparePlay(None))}
        >
            <img::Play />
        </Button>
    }
}
