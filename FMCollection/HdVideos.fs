module HdVideos

open FSharp.Data
open System.Text.RegularExpressions

type Video = { Name: string; Url: string }

let private readVideoPage (auth: string * string) (url: string) : Video option =
    let source = Http.RequestString(url, headers = [ auth ])
    let document = HtmlDocument.Parse source

    let streamMatch =
        Regex.Match(source, @"^df_movie\[df_movie\.length\].*path:""(.+?)""", RegexOptions.Multiline)

    if streamMatch.Success then
        Some
            { Name = (Seq.head (document.CssSelect("h1"))).InnerText()
              Url = "https://fmcollection.us" + streamMatch.Groups.[1].Value }
    else
        None

let getAvailable (auth: string * string) : seq<Video> =
    let category =
        Http.RequestString("https://fmcollection.us/members2/category.php?id=5", headers = [ auth ])
        |> HtmlDocument.Parse

    let links =
        Seq.map (fun (l: HtmlNode) -> l.Attribute("href").Value()) (category.CssSelect(".update-item h4 a"))

    Seq.choose (readVideoPage auth) links
