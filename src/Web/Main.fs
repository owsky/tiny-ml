module Web.Main

open Elmish
open Bolero
open Bolero.Html
open Components

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] HomePage
    | [<EndPoint "/type_inference">] TypeInferencePage
    | [<EndPoint "/syntax">] SyntaxPage

/// The Elmish application's model.
type Model = {
    page: Page
    TypeInference: TypeInference.Model
    error: string option
}

let initModel = {
    page = HomePage
    TypeInference = TypeInference.init
    error = None
}

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Error of exn
    | ClearError
    | TypeInferenceMsg of TypeInference.Message

let update message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | Error exn -> { model with error = Some exn.Message }, Cmd.none
    | ClearError -> { model with error = None }, Cmd.none
    | TypeInferenceMsg msg -> 
        let newModel, newCmd = TypeInference.update msg model.TypeInference
        { model with TypeInference = newModel }, newCmd

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

/// load template
type Main = Template<"wwwroot/main.html">

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model HomePage "Home"
            menuItem model SyntaxPage "Syntax"
            menuItem model TypeInferencePage "Type Inference"
        })
        .Body(
            cond model.page <| function
            | HomePage ->
                Home.view model dispatch
            | TypeInferencePage ->
                TypeInference.view model.TypeInference (dispatch << TypeInferenceMsg)
            | SyntaxPage ->
                Syntax.view model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
