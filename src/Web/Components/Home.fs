module Components.Home

open Bolero
open Bolero.Html

type Component() =
    inherit ElmishComponent<unit, unit>()

    override this.View _ _ =
        div {
            attr.``class`` "content box"
            h1 {
                attr.``class`` "title"
                text <| "Welcome to the web version of "
                a {
                    attr.href "https://github.com/owsky/tiny-ml"
                    attr.target "_blank"
                    text "TinyML"
                }
                text "!"
            }
            p {
                attr.``class`` "par"
                text "TinyML is a type inference program for a reduced version of Milner's "
                a {
                    attr.href "https://en.wikipedia.org/wiki/ML_(programming_language)"
                    attr.target "_blank"
                    text "ML"
                }
                text " programming language."
                br
                text "Syntactically, programs written in pure functional languages are expressions which contain other sub-expressions in a recursive fashion. Hence, it is possible to assign a type to a program, which is the type of the value returned by the program at the end of its execution."
                br
                text "In this case, programs are formed either by a single expression or a chain of LET bindings followed by one last expression. For example:"
            }
            div {
                attr.``class`` "box code-block"
                pre {
                    attr.``class`` "pre"
                    code {
                        attr.``class`` "code"
                        text "LET x = 5\nIN y = x\nIN x + y"
                    }
                }
                p {
                    text "This program has type int, as it returns an addition between integers."
                }
            }
        }