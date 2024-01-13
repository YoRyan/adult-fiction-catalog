open FSharp.Data

type ScrapeMode =
    | ScrapeHdVideos
    | ScrapeScenes

[<EntryPoint>]
let main argv =
    let username, password, mode =
        match argv with
        | [| a0; a1; "hd" |] -> a0, a1, ScrapeHdVideos
        | [| a0; a1; "scenes" |] -> a0, a1, ScrapeScenes
        | _ -> failwith "Expected arguments: <http username> <http password> hd|scenes"

    let auth = HttpRequestHeaders.BasicAuth username password

    match mode with
    | ScrapeHdVideos ->
        // Use with yt-dlp --add-headers
        let (_, authHeader) = auth
        printfn "Authorization: %s" authHeader

        for video in HdVideos.getAvailable auth do
            printfn "%s" (video.ToString())
    | ScrapeScenes -> Scenes.scrapeAll auth

    0
