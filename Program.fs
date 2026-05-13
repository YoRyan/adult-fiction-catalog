open Afc

[<EntryPoint>]
let main argv =
    let scraper, args =
        match List.ofArray argv with
        | [] -> failwith "Expected arguments: <scraper> [args...]"
        | scraper :: args -> scraper, args

    let main =
        match scraper with
        | "xsite" -> XSiteAbility.main
        | "shopmaker" -> Shopmaker.main
        | "paysite" -> Paysite.main
        | "fmc" -> FMCollection.Program.main
        | "c4s" -> Clips4sale.Program.main
        | _ -> failwith "Unknown scraper"

    main (List.toArray args)
