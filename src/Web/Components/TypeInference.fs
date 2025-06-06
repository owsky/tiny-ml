module Components.TypeInference

open Elmish
open Bolero
open Bolero.Html
open TinyML.Main
open Microsoft.AspNetCore.Components

let exampleProgramsMap = 
    Map.empty
        .Add("Constant", "5")
        .Add("Identity", "fun x -> x")
        .Add("Let Binding", sprintf "let x = 10 in\nx + 5")
        .Add("Application", sprintf "let f = fun x -> x + 1 in\nf 0")
        .Add("If Then Else", sprintf "let x = 3 in\nif x = 5 then x / 2 else x * 2")
        .Add("Tuples", sprintf "let f = fun x -> if x =. 2. then (x *. 3., true, 0) else (x /. 2., false, 1)\nin f 3.")

type Model = {
    sourceCode: string
    analysis: Result<string, string> option
    selectedExample: string
}

let init = {
    sourceCode = ""
    analysis = None
    selectedExample = ""
}

type Message =
    | SetSourceCode of string
    | ComputeAnalysis
    | SelectExample of string

let update message model =
    match message with
    | SetSourceCode code -> { model with sourceCode = code; selectedExample = "" }, Cmd.none
    | ComputeAnalysis -> 
        let output = analyzeCode model.sourceCode
        match output with
        | Ok res -> { model with analysis = Some (Ok (format_results true res)) }, Cmd.none
        | Error err -> { model with analysis = Some (Error (err)) }, Cmd.none
    | SelectExample exampleTitle ->
        let newSourceCode = Map.find exampleTitle exampleProgramsMap
        { model with selectedExample = exampleTitle; sourceCode = newSourceCode }, Cmd.none

let mySelect (model: Model) (dispatch: Dispatch<Message>) : Node =
    div {
        attr.``class`` "control"
        div {
            attr.``class`` "select"
            select {
                bind.change.string model.selectedExample (fun value ->
                    dispatch (SelectExample value)
                )
                for exampleProgram in Map.keys exampleProgramsMap do
                    option { 
                        attr.value (exampleProgram)
                        text (exampleProgram) 
                    }
            }
        }
    }

let createAnalysisResult (title: string) (content: string) =
    let replaceTabs (s: string) = s.Replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;")
    let boldHeadings = Array.map (fun (s: string) -> if s.EndsWith ":" then $"<strong>{s}</strong>" else s)
    let lines = content.TrimEnd('\n').Split("\n") |> boldHeadings
    let htmlContent = lines |> Array.map replaceTabs |> String.concat "<br />"
    div {
        attr.``class`` "content box mt-4"
        h1 {
            attr.``class`` "title"
            text title
        }
        div {
            attr.``class`` "textarea output"
            attr.rows <| lines.Length + 1
            rawHtml htmlContent
        }
    }

type Component() =
    inherit ElmishComponent<Model, Message>()

    override _.CssScope = CssScopes.TypeInference

    override this.View model dispatch =
        concat {
            div {
                attr.``class`` "content box"
                // Box title
                h1 {
                    attr.``class`` "title"
                    text "Type Inference"
                }
                // 
                div {
                    attr.``class`` "field is-grouped is-align-items-center"
                    div {
                        attr.``class`` "control"
                        label {
                            attr.``class`` "label"
                            attr.style "margin-right: 0.5rem; margin-bottom: 0;"
                            text "Example program:"
                        }
                    }
                    div {
                        attr.``class`` "control"
                        mySelect model dispatch
                    }
                }
                div {
                    attr.``class`` "field"
                    div {
                        attr.``class`` "control"
                        textarea {
                            attr.``class`` "textarea"
                            attr.placeholder "Enter your code here..."
                            attr.name "source code text area"
                            bind.change.string model.sourceCode (dispatch << SetSourceCode)
                        }
                    }
                }
                div {
                    attr.``class`` "field"
                    div {
                        attr.``class`` "control"
                        button {
                            attr.``class`` "button"
                            attr.id "infer_btn"
                            on.click (fun _ -> if model.sourceCode.Length <> 0 then dispatch ComputeAnalysis)
                            text "Infer Type"
                        }
                    }
                }
            }
            cond model.analysis <| function
                | None -> empty()
                | Some (Ok analysis) -> createAnalysisResult "Inferred Type" analysis
                | Some (Error err) -> createAnalysisResult "Error" err
        }