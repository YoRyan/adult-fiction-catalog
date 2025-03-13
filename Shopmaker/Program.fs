open FSharp.Data
open System
open System.IO
open System.Text.RegularExpressions
open YamlDotNet.Serialization

let userAgent =
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"

type Collection =
    { Meta:
        {| Title: string
           Models: string list
           Categories: string list
           Tags: string list
           Text: string |}
      Downloads: string list }

let toNtfsFilename s =
    Regex.Replace(s, @"[/\\:\*""\?<>\|]", "_")

let toBaseUrl url =
    (new Uri(url)).GetLeftPart(UriPartial.Authority)

let scrapeCollection (cookieHeader: string) (url: string) : Collection =
    let source =
        Http.RequestString(url, headers = [ "User-Agent", userAgent; "Cookie", cookieHeader ])

    let document = HtmlDocument.Parse source

    let getInnerText = fun (n: HtmlNode) -> n.InnerText()

    { Meta =
        {| Title =
            document.CssSelect("h1")
            |> List.map getInnerText
            |> String.concat ""
            |> (fun s -> s.Trim())
           Models = document.CssSelect(".models a") |> List.map getInnerText
           Categories =
            [ document.CssSelect("[title=Categories] a")
              document.CssSelect("[title=Category] a") ]
            |> List.concat
            |> List.map getInnerText
           Tags = document.CssSelect(".tags a") |> List.map getInnerText
           Text =
            document.CssSelect(".custom_text p")
            |> List.map getInnerText
            |> List.map (fun s -> s.Trim())
            |> String.concat "\n" |}
      Downloads =
        document.CssSelect(".available_qualities tbody")
        |> List.map (fun n -> n.CssSelect("tr")[1..1])
        |> List.concat
        |> List.map (fun n -> n.CssSelect("a"))
        |> List.concat
        |> List.choose (fun n -> n.TryGetAttribute("href"))
        |> List.map (fun a -> a.Value())
        |> List.map (fun s -> toBaseUrl url + s) }

let writeCollectionMeta (c: Collection) =
    let serializer = (new SerializerBuilder()).Build()
    let yaml = serializer.Serialize(c.Meta)

    let di = Directory.CreateDirectory(toNtfsFilename c.Meta.Title)
    File.WriteAllText(Path.Combine(di.FullName, "Metadata.yml"), yaml)

let collectionDownloads (cookieHeader: string) (c: Collection) : seq<string> =
    c.Downloads
    |> Seq.map (fun url ->
        $"{url}\n\tdir={toNtfsFilename c.Meta.Title}\n\theader=Cookie: {cookieHeader}\n\tauto-file-renaming=false")

let rec scrapeList (cookieHeader: string) (url: string) : seq<Collection> =
    seq {
        let baseUrl = toBaseUrl url

        let source =
            Http.RequestString(url, headers = [ "User-Agent", userAgent; "Cookie", cookieHeader ])

        let document = HtmlDocument.Parse source

        yield!
            document.CssSelect("h2 a")
            |> List.choose (fun n -> n.TryGetAttribute("href"))
            |> List.map (fun a -> a.Value())
            |> List.map (fun s -> baseUrl + s)
            |> Seq.map (scrapeCollection cookieHeader)

        let nextUrl =
            document.CssSelect("nav .next_page a")
            |> Seq.tryHead
            |> Option.bind (fun n -> n.TryGetAttribute("href"))
            |> Option.map (fun a -> a.Value())
            |> Option.bind (function
                | "#" -> None
                | s -> Some(baseUrl + s))

        yield!
            match nextUrl with
            | Some u -> scrapeList cookieHeader u
            | None -> Seq.empty
    }

[<EntryPoint>]
let main argv =
    let cookies, url =
        match argv with
        | [| a0; a1 |] -> a0, a1
        | _ -> failwith "Expected arguments: <cookies> <start url>"

    use ariaFile = File.CreateText("Shopmaker-aria2.txt")

    for c in (scrapeList cookies url) do
        writeCollectionMeta c

        for line in collectionDownloads cookies c do
            ariaFile.WriteLine line
            ariaFile.Flush()

        printfn "%s (%d)" c.Meta.Title c.Downloads.Length

    0
