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

    let app =
        choose [
            GET >=> choose
                [ path "/" >=> Files.browseFileHome "Index.html"
                ]
            POST >=> choose
                [ path "/" >=> Files.browseFileHome "Index.html"
                ]
            GET >=> Files.browseHome
            RequestErrors.NOT_FOUND "Page not found."  
        ]

    let listening, server = startWebServerAsync conf (app)
    
    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
    
    cts.Cancel()

    0