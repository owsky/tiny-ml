module Components.CheckBox

open Bolero
open Bolero.Html

type Model = {
    isChecked: bool
    label: string
}

type Message =
    | SetChecked of bool

type Component() =
    inherit ElmishComponent<Model, Message>()

    override _.CssScope = CssScopes.CheckBox

    override this.View model dispatch =
        div {
            attr.``class`` "field"
            div {
                attr.``class`` "control"
                label {
                    attr.``class`` "custom-checkbox"
                    span {
                        attr.``class`` "label custom-label"
                        text model.label
                    }
                    input {
                        attr.``type`` "checkbox"
                        bind.checked model.isChecked (dispatch << SetChecked)
                    }
                    span {
                        attr.``class`` "checkmark"
                    }
                }
            }
        }