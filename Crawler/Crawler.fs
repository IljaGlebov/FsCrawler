module Crawler

open System
open System.Collections.Concurrent
open System.Text.RegularExpressions
open System.Text
open System.Net
open System.Threading
open System.IO
open System.Collections.Generic


module Directory = 
    open System.IO
    
    let prepare s = 
        if Directory.Exists s then 
            Directory.Delete (s, true)

        Directory.CreateDirectory s |> ignore

type ContentType = | Html of Encoding | Image of string | Css | JavaScript | Plain 


type Content = Text of String | Data of byte[]

type Response = Response of ContentType * Content

let queue<'T>() = new BlockingCollection<'T>(ConcurrentQueue<_>())


[<RequireQualifiedAccess>]
type Status = 
    | Queued
    | Downloaded of Response
    | Parsed of Uri list
    | Stored of Uri list
    | SkipOrError

type UrlData = Status * int 

type HierarhyQueue(depth : int) = 
    let urls  = queue<Uri>()
    let parse = queue<Uri * Response>()
    let store = queue<Uri * Response>()

    let monitor = new Object()

    let tasks = Dictionary<Uri, UrlData>()

    let (|Resource|_|) s = 
        match s with 
        | Html _, _ -> None
        | _ -> Some s
        
    let rec completed url = 
        match tasks.TryGetValue url with
        | true, (Status.Stored [], _) 
        | true, (Status.SkipOrError, _)  -> true
        | true, (Status.Stored lst, _) -> lst |> List.forall completed
        | _ -> false

    let checkIsComplete() = 
        let top = tasks |> Seq.find(fun x -> match x.Value with (_, level) -> level = 0)
        
        if completed top.Key then
            urls.CompleteAdding()
            store.CompleteAdding()
            parse.CompleteAdding()

    let level uri = 
        match tasks.TryGetValue uri with
        | true, (_, level) -> level
        | _ -> 0

    let add l uri  = 
        if level uri <= depth && not <| tasks.ContainsKey uri  then
            tasks.Add(uri, (Status.Queued, l))
            uri |> urls.Add 
        
    let catchInvalidOperation f =
        try
            f() |> Some
        with
        | :? InvalidOperationException -> None

    member this.IsCompleted 
        with get() = 
            let top = tasks |> Seq.find(fun x -> match x.Value with (_, level) -> level = 0)
            completed top.Key

    member this.IsPendingDownload with get() = not urls.IsCompleted
    member this.IsPendingParse with get() = not parse.IsCompleted
    member this.IsPendingStore  with get() = not store.IsCompleted
    

    member this.Start uri = 
        add 0 uri

    member this.Level = level

    member this.StartChildren uri list = 
        lock monitor (fun () -> 
            let next = 1 + level uri
        
            list |> List.iter (add next)

            match tasks.TryGetValue uri with 
            | true, (Status.Downloaded r, _) -> 
                let t = (uri, r)
                store.Add t
            | _ -> ()
            
            tasks.[uri] <- (Status.Parsed list, level uri)
        )

    member this.ToSkipped uri = 
        lock monitor (fun () -> 
            tasks.[uri] <- (Status.SkipOrError, level uri)
            checkIsComplete()
        )

    member this.ToDownloaded uri response = 
        lock monitor (fun () -> 
            let level = uri |> level

            match response with 
            | Response (Html _, _) when level < depth ->  
                let r = (uri, response)
                parse.Add r
            | _ -> 
                let r = (uri, response)
                store.Add r

            tasks.[uri] <- (Status.Downloaded response, level)
        )



    member this.ToStored uri = 
        lock monitor (fun() ->
            let status = 
                match tasks.[uri] with
                | (Status.Downloaded _, level) -> Status.Stored [], level
                | (Status.Parsed lst, level) -> Status.Stored lst, level 
                | _ -> failwith "incorrect status"

            tasks.[uri] <- status

            checkIsComplete()
        )


    member this.TryTakeUrl() = catchInvalidOperation(urls.Take)
    member this.TryTakeParse() = catchInvalidOperation(parse.Take)
    member this.TryTakeStore() = catchInvalidOperation(store.Take)

    member this.Exists uri = tasks.ContainsKey uri

        
    
module Parser = 
    let (|WellFormedAbsolute|_|) s = 
        if Uri.IsWellFormedUriString(s, UriKind.Absolute) then Some s else None 

    let (|WellFormedRelative|_|) s = 
        if Uri.IsWellFormedUriString(s, UriKind.Relative) then Some s else None 

    let (|Schema|_|) input =
        let m = Regex.Match(input, "(.*?):" ) 
        if (m.Success) then Some m.Groups.[1].Value else None  

    let tryCreate (uri : Uri) (relative : string) = 
        match Uri.TryCreate(uri, relative) with
        | (true, result) -> Some result
        | _ -> None
    
    let extract html host = 
        Regex.Matches(html, "(src|href)=\\\"(.*?)\\\"") 
        |> Seq.cast<Match>
        |> Seq.map(fun x -> x.Groups.[2].Value)
        |> Seq.filter(function | Schema "http"
                                | Schema "https" -> true
                                | Schema _ -> false
                                | _ -> true)
        |> Seq.choose(function | WellFormedAbsolute url -> Uri(url) |> Some
                                | WellFormedRelative url -> tryCreate host url
                                | _ -> None)
        |> Seq.filter (fun uri -> String.IsNullOrEmpty uri.Fragment)
        


type Settings = {
    Host : Uri
    Depth : int
    SingleHost : bool
    ResultPath : string
} with static member Default = { Host = Uri("http://cone-forest.ru"); Depth = 1; SingleHost = true; ResultPath = "Cone-forest" }

type IWebClient = 
    abstract DownloadData : Uri -> byte[]
    abstract DownloadString : Uri -> Encoding -> string
    abstract ContentType : Uri -> ContentType option

type IDisk = 
    abstract Prepare : string -> unit
    abstract WriteData : string -> byte[] -> unit
    abstract WriteText : string -> string -> unit
    abstract WriteEncodedText : string -> string -> Encoding -> unit

type WebClient() =  
    let client = new System.Net.WebClient()

    do
        client.Proxy <- new System.Net.WebProxy()
        client.Headers.["User-Agent"] <- "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"

    interface IWebClient with 
        member this.DownloadData uri = client.DownloadData uri
        member this.DownloadString uri encoding = 
            client.Encoding <- encoding
            client.DownloadString uri

        // Тип определяется по запросу HEAD
        member this.ContentType uri = 
            let (|ParseResponse|_|) (s : string) = 
                let m = Regex.Match(s, "((text|application|image)\/(.*?))(;|$)")
            
                if m.Success then 
                    Some m.Groups.[1].Value
                else
                    None

            let toContentType (http: HttpWebResponse) =
                match http.ContentType with
                | ParseResponse "application/javascript" -> ContentType.JavaScript |> Some
                | ParseResponse "image/gif" -> ContentType.Image "gif"             |> Some
                | ParseResponse "image/jpeg" 
                | ParseResponse "image/pjpeg" -> ContentType.Image "jpeg"           |> Some
                | ParseResponse "image/png" -> ContentType.Image "png"             |> Some
                | ParseResponse "image/tiff" -> ContentType.Image "tiff"           |> Some
                | ParseResponse "text/css" -> ContentType.Css                      |> Some
                | ParseResponse "text/html" ->  Encoding.GetEncoding http.CharacterSet |> ContentType.Html |> Some
                | ParseResponse "text/plain" -> ContentType.Plain                  |> Some
                | _ -> None

            let request = System.Net.WebRequest.Create(uri);
            request.Method <- "HEAD"
            request.Timeout <- 5000
            try
                use response = request.GetResponse() 
                match response with 
                | :? HttpWebResponse as http -> 
                    http |> toContentType
                | _ -> None

            with
            | e -> None

type Disk() = 
    interface IDisk with
        member x.WriteData path content = File.WriteAllBytes(path, content)
        member x.WriteEncodedText path content enc = File.WriteAllText(path, content, enc)
        member x.WriteText path content = File.WriteAllText(path, content)
        member this.Prepare s = Directory.prepare s
        
    

type Crawler(settings : Settings, clientFactory : unit -> IWebClient, disk : IDisk) = 
    let queue  = HierarhyQueue(settings.Depth)

    let fileName = 
        let index = ref 0
        function | Html _ -> Interlocked.Increment index |> sprintf "%d.html" 
                 | Image p -> (Interlocked.Increment index, p) ||> sprintf "%d.%s" 
                 | Css -> Interlocked.Increment index |> sprintf "%d.css" 
                 | JavaScript -> Interlocked.Increment index |> sprintf "%d.js"
                 | Plain -> Interlocked.Increment index |> sprintf "%d.txt"

    
    // Загрузка
    let download() = 
        let client = clientFactory()

        let download (uri : Uri) ct = 
            try
                match ct with
                | Image _  ->  
                    let data = client.DownloadData uri
                    Response(ct, Data data) |> Some
                | Html enc ->
                    let html = client.DownloadString uri enc
                    Response(ct, Text html) |> Some
                | _ -> 
                    let html = client.DownloadString uri Encoding.UTF8
                    Response(ct, Text html) |> Some
            with 
                | _ -> None
            
        while queue.IsPendingDownload do
            match queue.TryTakeUrl() with
            | Some uri ->
                printfn "%d :  %s" (queue.Level uri) (uri.ToString())
                match uri |> client.ContentType |> Option.bind (download uri) with 
                | Some r -> r |> queue.ToDownloaded uri
                | None -> queue.ToSkipped uri
            | _ -> ()

    // Поиск ссылок
    let parse() = 
        while queue.IsPendingParse do
            match queue.TryTakeParse() with 
            | Some (host, Response(Html _, Text html)) ->
                let links = Parser.extract html host
                            |> Seq.filter (fun uri -> not <| queue.Exists uri)
                            |> Seq.filter (fun uri -> not settings.SingleHost || uri.Host = settings.Host.Host) 
                            |> Seq.toList

                queue.StartChildren host links
            | _ -> ()

    // Сохранение
    let store() = 
        while queue.IsPendingStore do
            match queue.TryTakeStore() with
            | Some (url, Response(ct, content) as resp)->
                let path = Path.Combine(settings.ResultPath, fileName(ct))

                match ct, content with 
                | Html(encoding), Text content ->
                    disk.WriteEncodedText path content encoding
                | _, Text content ->
                    disk.WriteText path content
                | _, Data data ->
                    disk.WriteData path data

                queue.ToStored url
            | t -> ()

    member this.Start() = 
        // https
        System.Net.ServicePointManager.ServerCertificateValidationCallback <- System.Net.Security.RemoteCertificateValidationCallback(fun _ _ _ _-> true)

        settings.ResultPath |> disk.Prepare
        queue.Start settings.Host

        let downloads = [0..19] |> List.map(fun _ -> Thread(download))  
        let parseThread = Thread(parse); 
        parseThread.Start()

        Thread(store).Start()
        
        downloads |> List.iter(fun t -> t.Start())
        parseThread.Join()

    new(settings) = Crawler(settings, (fun () -> WebClient() :> IWebClient), new Disk())


