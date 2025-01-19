open CliWrap
open FSharp.Data
open System
open System.Net
open System.Text.RegularExpressions

type Stream = { Title: string; Url: string }

let rec scrapePage (cc: CookieContainer) (url: string) : seq<Stream> =
    eprintfn "> %s" url

    let results = HtmlDocument.Parse(Http.RequestString(url, cookieContainer = cc))
    let body = results.Body()

    match body.CssSelect(".cloudflare-player") with
    | [ player ] ->
        let title = body.CssSelect("h1").Head.InnerText().Trim()
        let videoUrl = player.Attribute("src").Value()
        [ { Title = title; Url = videoUrl } ]
    | _ ->
        match body.CssSelect(".allVideos") with
        | [ videos ] ->
            let videoUrls =
                videos.CssSelect("h3 a") |> List.map (fun a -> a.Attribute("href").Value())

            let pageMatch = Regex.Match(url, @"\?.*page=(\d+)")

            let nextUrl =
                if pageMatch.Success then
                    let pageNum = int pageMatch.Groups[1].Value
                    Regex.Replace(url, @"page=\d+", $"page={pageNum + 1}")
                elif url.Contains("?") then
                    $"{url}&page=2"
                else
                    $"{url}?page=2"

            seq {
                yield! videoUrls |> Seq.map (fun url -> scrapePage cc url) |> Seq.concat

                if videoUrls.Length > 0 then
                    yield! scrapePage cc nextUrl
            }
        | _ -> []

let toYtDlp (dl: Stream) : list<string> =
    let ntfsTitle = Regex.Replace(dl.Title, @"[/\\:\*""\?<>\|]", "_")
    [ "--fragment-retries"; "1"; dl.Url; "--output"; $"{ntfsTitle}.%%(ext)s" ]

[<EntryPoint>]
let main argv =
    let url, cookie =
        match argv with
        | [| a0; a1 |] -> a0, a1
        | _ -> failwith "Expected arguments: <url> <http cookie>"

    let cookieName, cookieValue =
        match cookie.Split("=", 2) with
        | [| n; v |] -> n, v
        | _ -> failwith "Expected cookie format: name=value"

    let cc = CookieContainer()
    cc.Add(Cookie(cookieName, cookieValue, "/", Uri(url).Host))

    use stdout = Console.OpenStandardOutput()
    use stderr = Console.OpenStandardError()

    for s in scrapePage cc url do
        let t =
            task {
                try
                    let! result =
                        Cli
                            .Wrap("yt-dlp")
                            .WithArguments(toYtDlp s)
                            .WithStandardOutputPipe(PipeTarget.ToStream(stdout))
                            .WithStandardErrorPipe(PipeTarget.ToStream(stderr))
                            .ExecuteAsync()

                    ()
                with _ ->
                    ()
            }

        t.Wait()

    0
