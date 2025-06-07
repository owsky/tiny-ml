module Web.Routing

open Bolero

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] HomePage
    | [<EndPoint "/type_inference">] TypeInferencePage
    member this.Label =
        match this with
        | HomePage -> "Home"
        | TypeInferencePage -> "Type Inference"

let allPages = [ HomePage; TypeInferencePage]

type NavItem = {
    Label: string
    Url: string
    IsActive: bool
}