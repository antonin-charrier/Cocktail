open System
open System.Threading
open Suave
open Suave.Successful
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
              path "/test/" >=> OK "test GET"
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