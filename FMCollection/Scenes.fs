module Scenes

open FSharp.Data
open ReverseMarkdown
open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type private Page =
    | Category of links: seq<string>
    | Scene of slug: string * title: string * keywords: list<string> * description: string * downloads: list<string>
    | Unknown

let private scrapedFileName = "FMCScraped.txt"

let private startUrls =
    [ "https://fmcollection.us/members2/pages.php?id=categories&site_id=14"
      "https://fmcollection.us/members2/category.php?id=15&s=n"
      "https://fmcollection.us/members2/category.php?id=16&s=n"
      "https://fmcollection.us/members2/category.php?id=17&s=n"
      "https://fmcollection.us/members2/category.php?id=18&s=n"
      "https://fmcollection.us/members2/category.php?id=88&s=n"
      "https://fmcollection.us/members2/category.php?id=87&s=n"
      "https://fmcollection.us/members2/category.php?id=82" ]

let private readScraped () : Set<string> =
    let lines =
        try
            File.ReadLines(scrapedFileName)
        with _ ->
            []

    Set(lines |> Seq.where (fun l -> not (String.IsNullOrWhiteSpace(l))))

let private writeScraped (scraped: Set<string>) : unit =
    File.WriteAllLines(scrapedFileName, scraped)

let private readPage (auth: string * string) (url: string) : Page =
    let source = Http.RequestString(url, headers = [ auth ])
    let document = HtmlDocument.Parse source

    let items =
        List.map (fun (l: HtmlNode) -> l.Attribute("href").Value()) (document.CssSelect(".thumb h4 a"))

    if not items.IsEmpty then
        let nextPageMatch =
            Regex.Match(source, @"<!-- Next Page Link -->\s*<a class=""pagenav"" href=""(.+?)""")

        let links =
            if nextPageMatch.Success then
                List.append items [ nextPageMatch.Groups.[1].Value ]
            else
                items

        Category(links)
    else
        let images =
            Regex.Matches(source, @"^ptx\[""Full Size""\].*src: ""(.+?)""", RegexOptions.Multiline)
            |> Seq.cast<Match>
            |> Seq.map (fun m -> "https://fmcollection.us" + m.Groups.[1].Value)
            |> List.ofSeq

        let videos =
            Regex.Matches(source, @"^movie\[.*?path:""(.+?)"",  showplay:'0'", RegexOptions.Multiline)
            |> Seq.cast<Match>
            |> Seq.map (fun m -> "https://fmcollection.us" + m.Groups.[1].Value)
            |> List.ofSeq

        let downloads = images @ videos

        if downloads.IsEmpty then
            Unknown
        else
            let slugMatch = Regex.Match(url, @"/([^/]+?)(\.html)?$")
            let slug = slugMatch.Groups.[1].Value

            let title = (Seq.head (document.CssSelect("h1"))).InnerText()

            let findKeywords =
                function
                | HtmlElement("meta", attributes, _) ->
                    if
                        Seq.exists (fun (a: HtmlAttribute) -> a.Name() = "name" && a.Value() = "keywords") attributes
                    then
                        let content = Seq.find (fun (a: HtmlAttribute) -> a.Name() = "content") attributes
                        Some(content.Value())
                    else
                        None
                | _ -> None

            let keywords =
                List.ofArray ((Seq.pick findKeywords (document.CssSelect("meta"))).Split(","))

            let descriptionMatch = Regex.Match(source, @"<!--Description-->([\s\S]+?)<!--")

            let description =
                if descriptionMatch.Success then
                    let html = descriptionMatch.Groups.[1].Value

                    Converter(new Config(RemoveComments = true, UnknownTags = Config.UnknownTagsOption.Bypass))
                        .Convert(html)
                else
                    ""

            Scene(slug, title, keywords, description, downloads)

let directoryName (slug: string) (title: string) =
    Regex.Replace($"{slug} - {title}", @"[<>:""/\\|?*]", "_")

let rec downloadWithRetries (auth: string * string) (retries: int) (url: string) (outputPath: string) =
    let retry =
        using (File.OpenWrite(outputPath)) (fun file ->
            try
                let response = Http.RequestStream(url, headers = [ auth ])
                response.ResponseStream.CopyTo(file)
                false
            with _ ->
                if retries <= 0 then
                    reraise ()

                true)

    if retry then
        downloadWithRetries auth (retries - 1) url outputPath

let scrapeAll (auth: string * string) =
    let mutable scraped = readScraped ()
    let urls = new Queue<string>(startUrls)

    while urls.Count > 0 do
        let url = urls.Dequeue()

        if not (Set.contains url scraped) then
            match readPage auth url with
            | Category links ->
                for link in links do
                    urls.Enqueue(link)
            | Scene(slug, title, keywords, description, downloads) ->
                let dir = directoryName slug title
                Directory.CreateDirectory(dir) |> ignore

                File.WriteAllLines(Path.Combine(dir, "Keywords.txt"), keywords)

                if not (String.IsNullOrWhiteSpace(description)) then
                    File.WriteAllText(Path.Combine(dir, "Description.md"), description)

                let downloadFailed =
                    (Seq.exists
                        (fun downloadUrl ->
                            let filePath = Path.Combine(dir, Regex.Replace(downloadUrl, "^.+/", ""))
                            let retries = 3

                            try
                                downloadWithRetries auth retries downloadUrl filePath
                                false
                            with ex ->
                                printfn "Failed to download %s" downloadUrl
                                printfn "%s" (ex.ToString())
                                true)
                        downloads)

                if not downloadFailed then
                    // Downloads successfully scraped
                    printfn "%s (%d)" dir downloads.Length
                    scraped <- Set.add url scraped
                    writeScraped scraped
            | Unknown -> ()
