module Components.CodeBlock

open Bolero
open Bolero.Html

type Model = {
    sourceCode: string
}

let init = {
    sourceCode = ""
}

let replaceTabs (s: string) = s.Replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;")
let boldHeadings = Array.map (fun (s: string) -> if s.EndsWith ":" then $"<strong>{s}</strong>" else s)

type Component() =
    inherit ElmishComponent<Model, unit>()

    override _.CssScope = CssScopes.CodeBlock

    override this.View model _ =
        let lines = model.sourceCode.TrimEnd('\n').Split("\n") |> boldHeadings
        let htmlContent = lines |> Array.map replaceTabs |> String.concat "<br />"
        div {
            attr.``class`` "textarea output"
            attr.rows <| lines.Length + 1
            rawHtml htmlContent
        }