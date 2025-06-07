module Components.Home

open Bolero
open Bolero.Html

type Component() =
    inherit ElmishComponent<unit, unit>()

    override this.View _ _ =
        concat {
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
                    text "TinyML is a type inference program for a reduced version of Milner's "
                    a {
                        attr.href "https://en.wikipedia.org/wiki/ML_(programming_language)"
                        attr.target "_blank"
                        text "ML"
                    }
                    text " programming language. I developed this project as part of the Functional Languages course I took during my Master's studies."
                }
                p {
                    text "As per the requirements provided by the professor, the core library was written in F# thus, in order to provide this showcase, "
                    text "I needed to find a way to neatly execute the code on a static frontend. For this task I chose to use "
                    a {
                        attr.href "https://fsbolero.io/"
                        attr.target "_blank"
                        text "Bolero"
                    }
                    text ", which is a set of open source libraries that enable the development of web applications with F#."
                }
                p {
                    text "You may have noticed that this isn't the fastest website to load, the reason is that the web app needs to be loaded client-side using the"
                    text "WebAssembly bundle created by Bolero. A server mode with server-side rendering is supported but such runtime would be incompatible with "
                    text "free hosting services like "
                    a {
                        attr.href "https://pages.cloudflare.com/"
                        attr.target "_blank"
                        text "CloudFlare Pages"
                    }
                    text "."
                }
                p {
                    text "Within my "
                    a {
                        attr.href "https://github.com/owsky/tiny-ml"
                        attr.target "_blank"
                        text "repository"
                    }
                    text " I have provided a CLI application as well which ingests the source code through a file."
                }
            }
            div {
                attr.``class`` "content box"
                h1 {
                    attr.``class`` "title"
                    text "TinyML Syntax"
                }
                p {
                    text "Syntactically, programs written in pure functional languages are expressions which contain other sub-expressions in a recursive fashion."
                    text " Hence, it is possible to assign a type to a program, which is the type of the value returned by the program at the end of its execution."
                }
                p {
                    text "TinyML programs are formed either by a single expression or a chain of let..in bindings. For example, the following program has type int,"
                    text " since it returns the result of an addition between two integers:"
                }
                ecomp<CodeBlock.Component,_,_> { sourceCode = "let x = 5 in\nlet y = x in\nx + y" } ignore { attr.empty() }
                p {
                    text "The supported primitives are int, float and bool. Tuples and simple arrow types (functions) are also available. The following is an example"
                    text " of a function returning a tuple:"
                }
                ecomp<CodeBlock.Component,_,_> { sourceCode = "let f = fun (x: bool) -> (x, x or true) in\nf false" } ignore { attr.empty() }
                p {
                    text "The type annotation is optional, in fact if not provided the program will treat the function parameter as generic and it will automatically"
                    text " infer its type through the usage."
                }
                p {
                    text "The parser is very limited as it only supports functions with one parameter, which must be named 'x'."
                }
                p {
                    text "Basic arithmetic operations are supported, with distinct operators for integers and floating point numbers, i.e., float operators feature a"
                    text "dot suffix. This limitation was imposed to facilitate type inference."
                }
                ecomp<CodeBlock.Component,_,_> { sourceCode = "let x = 1 + 2 in\nlet y = 1. +. 3.1 in\n(x, y)" } ignore { attr.empty() }
                p {
                    text "As is customary for functional languages, if-then-else constructs are defined as expressions rather than statements. This means that the"
                    text "output can be directly assigned to a variable:"
                }
                ecomp<CodeBlock.Component,_,_> { sourceCode = "let x = 5 in\nlet y = if x <=5 then x * 2 else x in\ny" } ignore { attr.empty() }
            }
        }