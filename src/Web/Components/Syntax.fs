module Components.Syntax

open Bolero

/// load the template
type Syntax = Template<"wwwroot/syntax.html">

let view model dispatch = Syntax.Syntax().Elt()