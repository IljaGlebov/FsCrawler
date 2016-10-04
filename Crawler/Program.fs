module Programm 

open Crawler

let parseSettings (lst : string[]) = 
    let (|Param|_|) (input : string) =
        let m = input.Split('=')
        match m with 
        | [|k;v|] -> Some(k, v)
        | _ -> None

    let (|Integer|_|) s = 
        match System.Int32.TryParse s with
        | true, i -> Some i
        | _ -> None

    (Settings.Default, lst) 
    ||> Array.fold (fun set str -> match str with 
                                   | Param ("Host", host) -> {set with Host = System.Uri(host)}
                                   | Param ("Depth", Integer depth) -> {set with Depth = depth}
                                   | Param ("SingleHost", s) -> {set with SingleHost = (s = "true")}
                                   | Param ("ResultPath", s) -> {set with ResultPath = s}
                                   | _ -> set )

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let crawler = argv |>  parseSettings |> Crawler
    crawler.Start()

    printfn "Complete"
    
    0 
