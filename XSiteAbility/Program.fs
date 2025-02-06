open FSharp.Data
open System.IO
open System.IO.Compression
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

let rec getSetsForPage (user: string) (page: int) : seq<XSet> =
    seq {
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

        let items =
            body.CssSelect(".album-view li")
            |> Seq.filter (fun li -> not (li.HasClass("feat")))

        let sets = items |> Seq.map readSet

        yield! sets

        if Option.isSome (Seq.tryHead sets) then
            yield! getSetsForPage user (page + 1)
    }

let getSetsForUser user = getSetsForPage user 1

let unzipArchive (path: string) : unit =
    let extractOk =
        try
            ZipFile.ExtractToDirectory(path, Directory.GetParent(path).FullName)
            true
        with _ ->
            false

    if extractOk then
        File.Delete(path)

let scrapeSet (username: string) (password: string) (set: XSet) : seq<string> =
    seq {
        let title =
            match set.Title with
            | "" -> "(Empty Title)"
            | t -> t.Trim()

        let di = Directory.CreateDirectory(title)
        File.WriteAllText(Path.Combine(di.FullName, "Description.txt"), set.Description)

        let previewExt = Regex.Match(set.PreviewURL, @"\.(\w+)$").Groups.[1].Value
        let previewPath = Path.Combine(di.Name, $"Preview.{previewExt}")
        yield $"{set.PreviewURL}\n\tout={previewPath}\n\tauto-file-renaming=false"

        // Retrieve photo album content, if present.
        yield
            $"https://{username}:{password}@xsiteability.com/x/users/{set.User}/content/download.php?set={set.Id}&type=photo"
            + $"\n\tdir={di.Name}\n\tauto-file-renaming=false"

        // Retrieve video content, if present.
        yield
            $"https://{username}:{password}@xsiteability.com/x/users/{set.User}/content/download.php?set={set.Id}&type=video"
            + $"\n\tdir={di.Name}\n\tauto-file-renaming=false"
    }

[<EntryPoint>]
let main argv =
    let user, username, password =
        match argv with
        | [| a0; a1; a2 |] -> a0, a1, a2
        | _ -> failwith "Expected arguments: <xsiteability user> <http username> <http password>"

    use ariaFile = File.CreateText("XSiteAbility-aria2.txt")

    for set in (getSetsForUser user) do
        for line in (scrapeSet username password set) do
            ariaFile.WriteLine(line)

        printfn "%s: %s" set.Id set.Title

    0
