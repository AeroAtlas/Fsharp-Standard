
let fizzbuzz num =
  match num%3, num%5 with 
  | 0,0 -> "fizzbuzz"
  | 0,_ -> "fizz"
  | _,0 -> "buzz"
  | _,_ -> num.ToString()

[1..100]
  |>List.map fizzbuzz
  |>List.iter (fun (s:string) -> printfn "%s" s)