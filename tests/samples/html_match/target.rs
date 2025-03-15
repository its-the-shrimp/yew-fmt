// config: yew.html_flavor="Ext"
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        match ctx.selected_tab() {
            0 => <div id="inputs">
                <Slider
                    key="tmp"
                    name="Tempo"
                    setter={emitter.reform(AppEvent::Bpm)}
                    min=30
                    max=240
                    postfix="BPM"
                    initial={self.comp.bps * 60}
                />
                <Slider
                    key="gain"
                    name="Master volume"
                    setter={emitter.reform(|x| AppEvent::MasterVolume(R32::from(x)))}
                    initial={self.volume()}
                />
                <div class="export-options">
                    <Button
                        name="Export the project"
                        class="wide"
                        help="Save the whole project as an audio file"
                        onclick={emitter.reform(|_| {
                            AppEvent::OpenPopup(
                                Popup::Export {
                                    format: ExportFormat::Wav,
                                    filename: "project.wav".into(),
                                    err_msg: default(),
                                }
                            )
                        })}
                    >
                        <span>{ "Export the project" }</span>
                    </Button>
                    <Button
                        name="Save the project"
                        help="All the patterns & inputs will be saved as they are"
                        onclick={emitter.reform(|_| {
                            AppEvent::OpenPopup(
                                Popup::Export {
                                    format: ExportFormat::Wavexp,
                                    filename: "project.wavexp".into(),
                                    err_msg: default(),
                                }
                            )
                        })}
                    >
                        <img::FloppyDisk />
                    </Button>
                </div>
            </div>,

            1 => <div class="horizontal-menu dark-bg">
                for input in &self.comp.inputs {
                    <AudioInputButton
                        playing={self.playback_ctx.played_input().is_some_and(|i| i.eq(input))}
                        name={input.get().map_or_default(|x| AttrValue::from(x.name().clone()))}
                        {input}
                        {emitter}
                        bps={self.comp.bps}
                        class="extend-inner-button-panel"
                    />
                }
                <Button
                    name="Add audio input"
                    onclick={emitter.reform(|_| AppEvent::StartInputAdd)}
                >
                    <img::Plus />
                </Button>
            </div>,

            x if x % 2 != 0 => <code>{ "That's odd" }</code>,

            // just to test it out
            tab_id => <p style="color: red">{ format!("Invalid tab ID: {tab_id}") }</p>,
        }
    }
}
