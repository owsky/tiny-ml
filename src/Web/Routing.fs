module Web.Routing

open Bolero

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] HomePage
    | [<EndPoint "/type_inference">] TypeInferencePage
    | [<EndPoint "/syntax">] SyntaxPage
    member this.Label =
        match this with
        | HomePage -> "Home"
        | SyntaxPage -> "Syntax"
        | TypeInferencePage -> "Type Inference"

let allPages = [ HomePage; SyntaxPage; TypeInferencePage]

type NavItem = {
    Label: string
    Url: string
    IsActive: bool
}