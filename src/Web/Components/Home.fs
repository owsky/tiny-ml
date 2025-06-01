module Components.Home

open Bolero

/// load the template
type Home = Template<"wwwroot/home.html">

let view model dispatch = Home.Home().Elt()