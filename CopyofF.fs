open System

[<EntryPoint>]

let main argv =
  printfn "Please put a number between 1 and 4000"
  let input = Console.ReadLine()
  match Int32.TryParse input with
  | false,_ -> printfn "%s is not valid integer." input 
  | true, number -> 
    match 1 <= number && number <= 4000 with
    | false -> printfn "You entered %i. Please enter a number between 1 and 4000" number
    | true -> 
      [1 .. number] 
      |> List.map(fun num -> (num, num % 3, num % 5) 
      >> function 
      | (_,0,0) -> "Fizzbuz"
      | (_,0,_) -> "Fizz"
      | (_,_,0) -> "Buzz"
      | (num,_,_) -> string num)
      |> String.concat "\n"
      |> printfn "Here is the output: \n %s"
  0