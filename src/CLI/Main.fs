module CLI.Main

open System
open TinyML.Main
open Argu

type Arguments =
    | [<MainCommand>] Source_Code of path: string
    | Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Source_Code _ -> "specify the path to the source code to analyze"
            | Verbose -> "whether the analysis output should be verbose"

let read_file file_name : Result<string, string> =
    try
        use fstr = new IO.FileStream(file_name, IO.FileMode.Open)
        use rd = new IO.StreamReader(fstr)
        Ok <| rd.ReadToEnd ()
    with
    | e -> Error $"{e.Message}"

[<EntryPoint>]
let main argv =
    let reader = EnvironmentVariableConfigurationReader() :> IConfigurationReader
    let parser = ArgumentParser.Create<Arguments>(programName = "TinyML")
    let args = parser.Parse(argv, configurationReader = reader)
    let source_code = args.GetResult Source_Code
    let verbose = args.Contains Verbose

    read_file source_code |> function
    | Error err ->
        printfn $"{err}"
        1
    | Ok program ->
        analyzeCode program |> function
        | Ok r -> 
            printfn "%s" (format_results verbose r)
        | Error e ->
            printfn $"{e}"
        0