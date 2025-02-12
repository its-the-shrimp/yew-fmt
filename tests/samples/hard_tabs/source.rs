// config: hard_tabs=true

impl Component for MyComponent {
    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            { match num {
                1 => html! {
                    {"1"}
                },
                _ => html! {
                    {"???"}
                }
            } }
        }
    }
}
