module Components.Syntax

open Bolero
open Bolero.Html

type Component() =
    inherit ElmishComponent<unit, unit>()

    override this.View _ _ =
        div {
            attr.``class`` "content box"
            h1 {
                attr.``class`` "title"
                text "TinyML Syntax"
            }
            p {
                text "The language's syntax is structured around expressions. Indeed, a program is nothing else than a chain of expressions not unlike Lambda calculus."
            }
        }