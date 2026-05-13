module Afc.Clips4sale.Program

open System.IO
open System.Text.RegularExpressions

open Newtonsoft.Json

open Afc.Clips4sale.Scrape

let saveToJson clips =
    let serialized = clips |> Seq.map JsonConvert.SerializeObject
    File.WriteAllLines("clips4scrape.json", serialized)

let saveToDir clips =
    let outDir url =
        let m = Regex.Match(url, "/studio/(\d+)/\d+/([^/]+)$")

        if m.Success then
            Path.Combine(string m.Groups.[1], string m.Groups.[2])
        else
            string (url.GetHashCode())

    let writeIfPopulated path (arr: byte array) =
        if arr.Length > 0 then
            File.WriteAllBytes(path, arr)

    let serializeSettings =
        new JsonSerializerSettings(
            ContractResolver = new IgnoreBinaryAttributesResolver(),
            Formatting = Formatting.Indented
        )

    let save clip =
        let dir = Path.Combine("clips4scrape", outDir clip.URL)
        Directory.CreateDirectory(dir) |> ignore
        writeIfPopulated (Path.Combine(dir, "preview.jpg")) (clip.PreviewImage)
        writeIfPopulated (Path.Combine(dir, "preview.mp4")) (clip.PreviewVideo)
        writeIfPopulated (Path.Combine(dir, "preview.gif")) (clip.PreviewGif)
        let serialized = JsonConvert.SerializeObject(clip, serializeSettings)
        File.WriteAllText(Path.Combine(dir, "metadata.json"), serialized)

    for clip in clips do
        save clip

let main (argv: string array) =
    // let url = "https://www.clips4sale.com/studio/..."
    let url = argv[0]
    saveToDir (scrape url)
    0
