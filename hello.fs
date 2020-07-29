// printfn "Hello World from F#"
// let mutable a = 10
// a <- 20

// let items = [1..5]
// List.append items [6]
// items

let prefix prefixStr baseStr = 
  prefixStr + ", " + baseStr

let names = ["David"; "Maria"; "Alex"]

let prefixWithHello = prefix "Hello"

let exclaim s = s + "!"

names
|> Seq.map prefixWithHello
|> Seq.map exclaim

prefix "Hello" "David"