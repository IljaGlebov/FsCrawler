module CrawlerTests

open NUnit.Framework
open FsUnit
open Crawler
open System
open System.Collections.Generic
open System.Threading

type StubClient() = 
    let contentTypes = Dictionary<Uri, ContentType option>()
    let data = Dictionary<Uri, byte[]>()
    let strings = Dictionary<Uri, string>()

    interface IWebClient with
        member this.ContentType url =
            Thread.Sleep 500 

            match contentTypes.TryGetValue url with
            | true,  ct -> ct
            | _ -> None

        member this.DownloadData url = 
            Thread.Sleep 500 
            data.[url]
        member this.DownloadString url _ = 
            Thread.Sleep 500 
            strings.[url]

    member this.AddHtml url content = 
        let uri = Uri(url)
        contentTypes.[uri] <- Text.Encoding.UTF8 |> ContentType.Html |> Some
        strings.[uri] <- content
            
    member this.AddJavaScript url content = 
        let uri = Uri(url)
        contentTypes.[uri] <- ContentType.JavaScript |> Some
        strings.[uri] <- content

    member this.AddPicture url = 
        let uri = Uri(url)
        contentTypes.[uri] <- "gif" |> ContentType.Image |> Some
        data.[uri] <- Array.create 100 (byte 1)

        

type StubDisk() = 
    let saved = List<string>()

    interface IDisk with
        member x.Prepare _ = ()
        member x.WriteData file _ = saved.Add file
        member x.WriteEncodedText file _ _ = saved.Add file
        member x.WriteText file _ = saved.Add file

    
    member x.StoredCount with get() = saved.Count
        
         
        

let factory c = fun () -> c :> IWebClient
let root = "http://test.com"

let content = """ <!DOCTYPE html>
                <html xmlns="http://www.w3.org/1999/xhtml"> 
                    <head>
                    <script type="text/javascript" src="https://ajax.aspnetcdn.com/ajax/jQuery/script2.js" data-do-not-move="true" data-provides="jQuery"></script>
                    <script type="text/javascript" src="script1.js" data-do-not-move="true" data-provides="jQuery"></script>
                </head>
                    <body> 
                        <img alt="logo" class="logo" src="https://i-msdn.sec.s-msft.com/Areas/Centers/Themes/StandardDevCenter/Content/HeaderFooterSprite.png?v=636106135500781575" />

                        <div class="linkList">
                            <ul class="links horizontal">
                              <li>
                                <a href="http://test.com/newsletter.aspx" id="BottomLinks_2148_7" xmlns="http://www.w3.org/1999/xhtml">Newsletter</a>
                              </li>
                              <li>
                                <a href="/cc300389" id="BottomLinks_2148_9" xmlns="http://www.w3.org/1999/xhtml">Terms of use</a>
                              </li>
                              <li>
                                <a href="https://www.microsoft.com/en-us/legal/intellectualproperty/Trademarks/" id="BottomLinks_2148_10" xmlns="http://www.w3.org/1999/xhtml">Trademarks</a>
                              </li>
                            </ul>
                          </div>
                    </body>
                </html>
                """

let content2 = """ <!DOCTYPE html>
                <html xmlns="http://www.w3.org/1999/xhtml"> 
                    <body> 

                        <div class="linkList">
                            <ul class="links horizontal">
                              <li>
                                <a href="http://test.com/newsletter.aspx" id="BottomLinks_2148_7" xmlns="http://www.w3.org/1999/xhtml">Newsletter</a>
                              </li>
                              <li>
                                <a href="/level2" id="BottomLinks_2148_9" xmlns="http://www.w3.org/1999/xhtml">Terms of use</a>
                              </li>
                            </ul>
                          </div>
                    </body>
                </html> """
let defaultSettings = {Settings.Default with Settings.Depth = 0; Host = Uri(root)}


let mutable disk = StubDisk()
let mutable client = StubClient()

[<SetUp>]
let setup() = 
    disk <- StubDisk()
    client <- StubClient()

    client.AddHtml root content    
    client.AddJavaScript (root + "/script1.js") "<script> </script>"
    client.AddJavaScript "https://ajax.aspnetcdn.com/ajax/jQuery/script2.js" "<script> </script>"
    client.AddPicture "https://i-msdn.sec.s-msft.com/Areas/Centers/Themes/StandardDevCenter/Content/HeaderFooterSprite.png?v=636106135500781575"
    client.AddHtml (root + "/newsletter.aspx")  "<html> </html>"
    client.AddHtml (root + "/cc300389") content2
    client.AddHtml (root + "/level2") "<html> </html>"
    client.AddHtml "https://www.microsoft.com/en-us/legal/intellectualproperty/Trademarks/" "<html> </html>"

[<Test>]
let ``When depth = 1 and single host expect (3 + 1) files downloded``() = 

    let crawler = Crawler({defaultSettings with Depth = 1}, client |> factory, disk)
    
    crawler.Start()

    disk.StoredCount |> should equal (3 + 1)


[<Test>]
let ``When depth = 0 expect single file downloded``() = 

    let crawler = Crawler(defaultSettings, client |> factory, disk)

    crawler.Start()

    disk.StoredCount |> should equal 1



[<Test>]
let ``When depth = 1 expect (6 + 1) files downloded``() = 

    let crawler = Crawler({defaultSettings with Depth = 1; SingleHost = false}, client |> factory, disk)
    
    crawler.Start()

    disk.StoredCount |> should equal (6 + 1)

[<Test>]
let ``When depth = 2 expect (6 + 1 + 1) files downloded``() = 

    let crawler = Crawler({defaultSettings with Depth = 2; SingleHost = false}, client |> factory, disk)
    
    crawler.Start()

    disk.StoredCount |> should equal (6 + 1 + 1)

[<Test>]
let ``When web error expect 1 files downloded``() = 
    client <- StubClient()
    client.AddHtml root content  

    let crawler = Crawler({defaultSettings with Depth = 1; SingleHost = false}, client |> factory, disk)
        
    crawler.Start()

    disk.StoredCount |> should equal 1

