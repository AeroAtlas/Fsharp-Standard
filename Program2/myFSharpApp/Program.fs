namespace MyFSharpApp

open System

module Program =
  [<EntryPoint>] //function that's entrypoint of application
  let main args = 
    printfn "Hi what is your name?"
    let name = Console.ReadLine () //() = unit
    printfn "hi %s" name

    let currentTime () =
      DateTime.Now 
    
    currentTime ()
    |> printfn "Time = %O"  
    0 //error code