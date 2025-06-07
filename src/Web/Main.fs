module Web.Main

open Elmish
open Bolero
open Bolero.Html
open Components
open Routing

/// The Elmish application's model.
type Model = {
    page: Page
    TypeInference: TypeInference.Model
}

let initModel = {
    page = HomePage
    TypeInference = TypeInference.init
}

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | TypeInferenceMsg of TypeInference.Message

let update message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | TypeInferenceMsg msg -> 
        let newModel, newCmd = TypeInference.update msg model.TypeInference
        { model with TypeInference = newModel }, newCmd

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

let view (model: Model) dispatch =
    let navItems =
        Routing.allPages
        |> List.map (fun page ->
            {
                Label = page.Label
                Url = router.Link page
                IsActive = model.page = page
            }
        )
    concat {
        ecomp<NavBar.NavBar,_,_>
            { Items = navItems }
            ignore
            { attr.empty() }
        section {
            attr.``class`` "section"
            cond model.page <| function
            | HomePage ->
                ecomp<Home.Component,_,_> () ignore { attr.empty() }
            | TypeInferencePage -> 
                ecomp<TypeInference.Component,_,_> model.TypeInference (TypeInferenceMsg >> dispatch) { attr.empty() }
        }
    }


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
