namespace Clips4Scrape

open FSharp.Data
open Newtonsoft.Json.Serialization
open System.Reflection
open System.Threading

module ScrapeModule =

    /// back off and retry time
    let retryMs = 30 * 1000

    type BinaryAttribute() =
        inherit System.Attribute()

    let binaryAttributeType = BinaryAttribute().GetType()

    type IgnoreBinaryAttributesResolver() =
        inherit DefaultContractResolver()

        override this.CreateProperty(memb, membSerialization) =
            let prop = base.CreateProperty(memb, membSerialization)
            let isBinaryAttr (cad: CustomAttributeData) = cad.AttributeType = binaryAttributeType

            if Seq.exists isBinaryAttr (memb.CustomAttributes) then
                prop.ShouldSerialize <- fun _ -> false

            prop

    type Clip =
        { URL: string

          Title: string
          DescriptionHTML: string

          [<BinaryAttribute>]
          PreviewImage: byte array
          [<BinaryAttribute>]
          PreviewVideo: byte array
          [<BinaryAttribute>]
          PreviewGif: byte array

          Category: string
          RelatedCategories: list<string>
          Keywords: list<string>

          Price: string
          Length: string
          Size: string
          Format: string
          Resolution: string
          Added: string }

    /// Download the contents of a file. Retry on network failures and abort
    /// (return empty array) on HTTP 4xx errors.
    let rec downloadFile url =
        let request =
            try
                Ok(Http.Request(url, silentHttpErrors = true))
            with e ->
                Error e

        match request with
        | Ok req ->
            let (|Ok|Retry|Abort|) code =
                if code = 200 then Ok
                else if code >= 400 && code < 500 then Abort
                else Retry

            match req.StatusCode with
            | Ok ->
                match req.Body with
                | Binary bytes -> bytes
                | Text text -> System.Text.Encoding.UTF8.GetBytes text
            | Retry ->
                Thread.Sleep(retryMs)
                downloadFile url
            | Abort -> Array.empty
        | Error _ ->
            Thread.Sleep(retryMs)
            downloadFile url

    /// .CssSelect() but for pipes
    let cssSelect css (node: HtmlNode) = node.CssSelect(css)

    /// .TryGetAttribute() but for pipes
    let tryAttribute attr (node: HtmlNode) = node.TryGetAttribute(attr)

    /// .Trim() but for pipes
    let trim (str: string) = str.Trim()

    /// Converts the value of a link attribute, which may contain a relative
    /// URL, to an absolute URL.
    let absoluteUrl (attr: HtmlAttribute) =
        match attr.Value() with
        | url when url.StartsWith("//") -> "https:" + url
        | url when url.StartsWith("/") -> "https://clips4sale.com" + url
        | url -> url

    /// Scrape a clip from an individual details page.
    let scrapeClip url (doc: HtmlDocument) =
        let makeClip cont =
            let title =
                cont
                |> cssSelect "h3"
                |> Seq.tryHead
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let desc =
                cont
                |> cssSelect ".individualClipDescription body"
                |> Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.map (Seq.map string)
                |> Option.map (String.concat "\n")
                |> Option.map trim

            let image =
                cont
                |> cssSelect ".clipImage"
                |> Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 2)
                |> Option.bind (tryAttribute "src")
                |> Option.map absoluteUrl
                |> Option.map downloadFile

            let video =
                cont
                |> cssSelect ".clipImage"
                |> Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.bind (tryAttribute "src")
                |> Option.map absoluteUrl
                |> Option.map downloadFile

            let gif =
                cont
                |> cssSelect ".clipImage"
                |> Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 2)
                |> Option.bind (tryAttribute "data-src")
                |> Option.map absoluteUrl
                |> Option.map downloadFile

            let category =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 0
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let related =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 0
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.defaultValue []
                |> List.map HtmlNode.elements
                |> List.map Seq.tryHead
                |> List.choose id
                |> List.map HtmlNode.innerText
                |> List.map trim

            let keywords =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 0
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 2)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.defaultValue []
                |> List.map HtmlNode.elements
                |> List.map Seq.tryHead
                |> List.choose id
                |> List.map HtmlNode.innerText
                |> List.map trim

            let price =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let length =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let size =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 2)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let format =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 0)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let resolution =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            let added =
                cont
                |> cssSelect ".clip_details"
                |> Seq.tryItem 1
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 2)
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind (Seq.tryItem 1)
                |> Option.map HtmlNode.innerText
                |> Option.map trim

            match title, desc with
            | Some titleStr, Some descStr ->
                Some
                    { URL = url

                      Title = titleStr
                      DescriptionHTML = descStr

                      PreviewImage = image |> Option.defaultValue Array.empty
                      PreviewVideo = video |> Option.defaultValue Array.empty
                      PreviewGif = gif |> Option.defaultValue Array.empty

                      Category = category |> Option.defaultValue ""
                      RelatedCategories = related
                      Keywords = keywords

                      Price = price |> Option.defaultValue ""
                      Length = length |> Option.defaultValue ""
                      Size = size |> Option.defaultValue ""
                      Format = format |> Option.defaultValue ""
                      Resolution = resolution |> Option.defaultValue ""
                      Added = added |> Option.defaultValue "" }
            | _ -> None

        Seq.tryHead (doc.CssSelect("#individualClip")) |> Option.bind makeClip

    /// Scrape clips from a search page.
    let rec scrapeSearch (doc: HtmlDocument) =
        let links =
            doc.Html()
            |> cssSelect ".clipWrapper"
            |> Seq.tryHead
            |> Option.map (cssSelect "a.clipTitleLink")
            |> Option.defaultValue []
            |> Seq.map (tryAttribute "href")
            |> Seq.choose id
            |> Seq.map absoluteUrl

        seq {
            for url in links do
                yield! scrape url
            // Scrape the next page, if any.
            let nextPage =
                doc.Html()
                |> cssSelect ".paginationNextBtn"
                |> Seq.tryHead
                |> Option.map HtmlNode.elements
                |> Option.bind Seq.tryHead
                |> Option.bind (tryAttribute "href")
                |> Option.map absoluteUrl

            if nextPage.IsSome then
                yield! scrape nextPage.Value
        }

    /// Scrape all clips from a Clips4Sale URL, which can be a search page or an
    /// individual clip page.
    and scrape url =
        printfn "VISIT %s" url

        let (|Search|_|) (doc: HtmlDocument) =
            if not (Seq.isEmpty (doc.CssSelect("#view-searchInfo"))) then
                Some(doc)
            else
                None

        let (|Clip|_|) (doc: HtmlDocument) =
            if not (Seq.isEmpty (doc.CssSelect("#individualClip"))) then
                Some(doc)
            else
                None

        let rec download () =
            try
                HtmlDocument.Load url
            with _ ->
                Thread.Sleep(retryMs)
                download ()

        match download () with
        | Search doc -> scrapeSearch doc
        | Clip doc ->
            match scrapeClip url doc with
            | Some(clip) -> Seq.singleton clip
            | None -> Seq.empty
        | _ -> Seq.empty
