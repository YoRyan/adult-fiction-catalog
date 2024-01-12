open FSharp.Data
open System
open System.IO
open System.IO.Compression
open System.Net.Http.Headers
open System.Text.RegularExpressions

// Scraper for vendors on the XSiteAbility platform
// This was written really quickly and a bunch of stuff is missing:
// - Free downloads
// - Any error handling whatsoever

type XSet =
    { Id: string
      User: string
      Title: string
      Description: string
      PreviewURL: string }

type Download = { Name: string; Bytes: byte array }

let scrapedFileName = "XSiteAbilityScraped.txt"

let readScraped () : Set<string> =
    let lines =
        try
            File.ReadLines(scrapedFileName)
        with _ ->
            []

    Set(lines |> Seq.where (fun l -> not (String.IsNullOrWhiteSpace(l))))

let writeScraped (scraped: Set<string>) : unit =
    File.WriteAllLines(scrapedFileName, scraped)

let rec getSetsForPage (user: string) (page: int) : seq<XSet> =
    let results =
        HtmlDocument.Load $"https://xsiteability.com/x-new/new-preview-grid.php?page={page}&user={user}"

    let body = results.Body()

    let readSet (li: HtmlNode) : XSet =
        let href = (Seq.head (li.Descendants "a")).Attribute("href").Value()

        { Id = Regex.Match(href, @"setid=(\d+)").Groups.[1].Value
          User = user
          Title = (Seq.head (li.Descendants "h4")).InnerText()
          Description = li.CssSelect(".setdesc").Head.InnerText()
          PreviewURL =
            "https://xsiteability.com/"
            + (Seq.head (li.Descendants "img")).Attribute("src").Value() }

    let items = body.CssSelect(".album-view li")
    let sets = items |> Seq.map readSet

    let nextSets =
        match Seq.tryHead sets with
        | Some _ -> getSetsForPage user (page + 1)
        | None -> []

    Seq.append sets nextSets

let getSetsForUser user = getSetsForPage user 1

let readDownloadResponse (response: HttpResponse) : Download option =
    match response.StatusCode with
    | 200 ->
        let fileName =
            match response.Headers.TryGetValue "Content-Disposition" with
            | true, header ->
                let contentDisp = ContentDispositionHeaderValue.Parse(header)
                Regex.Replace(contentDisp.FileName, @"^""(.*)""$", "$1")
            | _ -> ""

        match response.Body with
        | Binary bytes -> Some { Name = fileName; Bytes = bytes }
        | _ -> None // If we get a text response, it's probably an error message.
    | _ -> None

let unzipArchive (path: string) : unit =
    let extractOk =
        try
            ZipFile.ExtractToDirectory(path, Directory.GetParent(path).FullName)
            true
        with _ ->
            false

    if extractOk then
        File.Delete(path)

let scrapeSet (auth: string * string) (set: XSet) : unit =
    let di = Directory.CreateDirectory(set.Title)

    File.WriteAllText(Path.Combine(di.FullName, "Description.txt"), set.Description)

    let previewExt = Regex.Match(set.PreviewURL, @"\.(\w+)$").Groups.[1].Value

    match readDownloadResponse (Http.Request(set.PreviewURL, silentHttpErrors = true)) with
    | Some download -> File.WriteAllBytes(Path.Combine(di.FullName, $"Preview.{previewExt}"), download.Bytes)
    | None -> ()

    // Retrieve photo album content, if present.
    match
        readDownloadResponse (
            Http.Request(
                $"https://xsiteability.com/x/users/{set.User}/content/download.php",
                query = [ "type", "photo"; "set", set.Id ],
                headers = [ auth ]
            )
        )
    with
    | Some download ->
        let outPath = Path.Combine(di.FullName, download.Name)
        File.WriteAllBytes(outPath, download.Bytes)

        if Path.GetExtension(outPath).ToLower() = ".zip" then
            unzipArchive (outPath)
    | None -> ()

    // Retrieve video content, if present.
    match
        readDownloadResponse (
            Http.Request(
                $"https://xsiteability.com/x/users/{set.User}/content/download.php",
                query = [ "type", "video"; "set", set.Id ],
                headers = [ auth ]
            )
        )
    with
    | Some download -> File.WriteAllBytes(Path.Combine(di.FullName, download.Name), download.Bytes)
    | None -> ()

[<EntryPoint>]
let main argv =
    let user, username, password =
        match argv with
        | [| a0; a1; a2 |] -> a0, a1, a2
        | _ -> failwith "Expected arguments: <xsiteability user> <http username> <http password>"

    let auth = HttpRequestHeaders.BasicAuth username password
    let mutable scraped = readScraped ()

    for set in Seq.where (fun s -> not (Set.contains s.Id scraped)) (getSetsForUser user) do
        scrapeSet auth set

        scraped <- Set.add set.Id scraped
        writeScraped scraped

        printfn "%s: %s" set.Id set.Title

    0
