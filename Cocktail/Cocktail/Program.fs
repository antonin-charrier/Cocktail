open System
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let sourceDir = __SOURCE_DIRECTORY__
    let folder = "\Front"
    let conf = { defaultConfig with cancellationToken = cts.Token ; homeFolder = Some (sourceDir + folder) }

    let handlePost f ctx =
        async {
            let rawBody = ctx.request.rawForm
            let body = System.Text.Encoding.UTF8.GetString rawBody
            let result = f body
            return Some { ctx with response = { ctx.response with status = result; content = Bytes rawBody } }
        }

    let onMove body =
        //Extraire json et traiter le mouvement du mec 
        { reason = "OK"; code = 200 }

    let app =
      choose [
        GET >=> choose
            [ path "/" >=> Files.browseFileHome "Index.html"
            ]
        POST >=> choose
            [ path "/move" >=> handlePost onMove
            ]
        GET >=> Files.browseHome
        RequestErrors.NOT_FOUND "Page not found."
        ]

    let listening, server = startWebServerAsync conf (app)

    let onMove =
        fun (moves : String) -> 
            "a"

    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
    
    cts.Cancel()

    0