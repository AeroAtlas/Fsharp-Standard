//Record type - product types
//tuple
//Anonymous record
open System
//Record type //read only class with getters
type Day = {DayOfTheMonth: int; Month: int}
type Person = {Name: string; Age: int}
  with 
    static member (+) ({Name = n1; Age = a1},{Name = n2; Age = a2} )=
      {Name = n1 + n2; Age = a1 + a2}
//type Duo = {Person1: Person; Person2: Person}

let ben = {Name = "Ben"; Age = 26}
let alex = {Name = "Alex"; Age = 19}

ben.Name
ben.Age
ben > ben2

let incremenetAge person = 
  { person with Age = person.Age + 1}

incremenetAge ben

//let brothers = {Person1 = alex; Person2 = ben}

(ben,42) //tuple

let duo = {|Person1 = ben; Person2 = alex|}
let trio = {|duo with Person3 = ben|}



let yesOrNo bool = 
  // if bool //if statement
  //   then "Yes"
  //   else "No"
  match bool with //constant pattern matching
  | true -> "Yes"
  | false -> "No"

let yesOrNo' = function //use this when don't need var in scope
  | true -> "Yes"
  | false -> "No"

let yesOrNo'' bool = //same as this but with points. also don't do this
  (function 
  | true -> "Yes"
  | false -> "No")

let isEven = function
  | x when x % 2 = 0 -> true //<> = !=
  | _ -> false //_ is discard operator

let isEven'' = function
  | x when x % 2 = 0 -> true, x
  | x when x % 2 <> 0 -> false, x
  | _ -> false, 0 //not sure if correct

let isEven' x =
  x % 2 = 0

let isOne = function
  | 1 -> true
  | _ -> false 

let isOne'' (x:bool,y:int) =
  match (x,y) with
  | y (=) 1 -> true, y
  | y (<>) 1 -> false, y 
  | _ -> false, y


let isOne' number = number = 1
let isOne''' = (=) 1

50
|>isEven''
|>isOne''
|> printfn "%O"

let translateFizzBuzz = function
  | "Fizz" -> 3
  | "Buzz" -> 5
  | "FizzBuzz" -> 15
  | _ -> failwith "Not a correct argument"

seq [1;2;4]

translateFizzBuzz "Fizz"
translateFizzBuzz "Buzz"
translateFizzBuzz "FizzBuzz"
translateFizzBuzz "Fozz"

type NormalRectangle = {Base: double; Height: double}

//This works with single case pattern matches
//Record types
//Tuples
//Single Case DU

//wrapper types
type OrderId = OrderId of int
// type OrderId = OrderId of int ALT

let incrementOrderId (OrderId id) = 
  id + 1 
  |>OrderId
  //Order (id+1) ALT
  //OrderId <| id + 1 ALT2

let incremenetOrderId =
  fun (OrderId id) ->
    OrderId <| id + 1






let area {Base=b;Height=h} =
  b * h
let area' (b,h) = b * h

type Rectangle = 
  | Normal of NormalRectangle
  | Square of side:double

module Rectangle =
  let area = function
    | Normal {Base = b; Height = h} -> b * h
    | Square s -> s ** 2.

//Sum types / discriminated union
type Shape = //of is constructor of an object
  | Rectangle of Rectangle
  | Triangle of height:double * _base:double
  | Circle of radius:double
  | Dot

let circle = Circle 1. 
let triangle = Triangle (2.,4.)

module Shape =
  // let area shape = //pattern matching
  //   match shape with
  //   | Rectangle rect -> rect.Base * rect.Height
  //   //| Rectangle {Base = b; Height = h} -> b * h   //alternate
  //   | Triangle (h,b) -> h * b / 2. //() deconstruct tuple
  //   | Circle r -> r ** 2. * System.Math.PI
  //   | Dot -> 1.
  
  let area' = function //pattern matching
    | Rectangle rect -> Rectangle.area rect
    | Triangle (h,b) -> h * b / 2. //() deconstruct tuple
    | Circle r -> r ** 2. * System.Math.PI
    | Dot -> 1.

   

// Shape.area' circle

type Option<'a> =
  | Some of 'a
  | None //replaces null


let translateFizzBuzz' = function
  | "Fizz" -> Some 3
  | "Buzz" -> Some 5
  | "FizzBuzz" -> Some 15
  | _ -> None 

let hasValue = function 
  | Some _ -> true 
  | None -> false

let getValue = function 
  | Some x -> x
  | None -> 0

"Fizz"
|>translateFizzBuzz'
|>getValue
|> printfn "%i"

let ben' = {Name = "Ben"; Age = 26}
let alex' = {Name = "Alex"; Age = 19}

let inline add x y = x + y //inline better for static

let bob = ben' + alex'

add 3 5

//Array Fixed size and Mutable
[|1;2;3;4;5;6|]
let arr =[|
  1..4
|]

arr.[0] <- 5 //<- assignement operator 0 index of array is now 5  
arr

//List (Linked List) Immutable
[1;2;3;4;5]
let list = [1 .. 2 .. 14] //increments by 2
let list' = [1. .. 0.1 .. 10.] //increments by 0.1
let alist = ['a' .. 'z']

//this is what a list is
// type LinkedList<'a> =
//   | ([])
//   | (::) of head:'a * tail: 'a list // :: cons = constructing

let addToList x xs = 
  x::xs //x goes to font of list 

let sampleList = [2;3;4]
addToList 1 sampleList

let getFirstItem = function 
  | x::_ -> Some x 
  | _ -> None 

let getFirstItem' list =
  List.tryHead list

let x: int = List.head []

// let rec printEveryItem = function //rec is recursive
//   | x::xs -> 
//     printfn "%O" x
//     printEveryItem xs
//   | [] -> ()

let rec doWithEveryItem f = function //rec is recursive
  | x::xs -> 
    f x
    doWithEveryItem f xs
  | [] -> ()

let printEveryItem' list =
  doWithEveryItem (printfn "%O")



let printEveryItem' list =
  list 
  |> List.iter (printfn "%O") //Foreach

printEveryItem [1;2;3;4]


let stringifyList (list:int list) =
  list 
  |> List.map string

  
[1 .. 10] |>stringifyList
// Some "42"

[1..10] //sum of list
let sum list = 
  list 
  |> List.fold (fun total current -> total + current) 0

let reduce list =
  list
  |> List.reduce (+)

let inline sum'' list =
  List.sum list

let bob list=
  list 
  |> sum''

[1..10]
|>bob

let divideInteger nominator denominator =
  match nominator % denominator with
  | 0 -> Some <| nominator / denominator
  | _ -> None

let divideBy2 = divideInteger 2

let bind f  = function 
  | Some x -> f x 
  | None -> None 

24
|> divideBy2
|> Option.bind divideBy2

exception CannotConnectException of System.Uri 

let handle f = 
  try
    f () 
  with 
  | CannotConnectException uri -> ()
  | :? System.ArgumentException as e -> printfn "%s" e.Message // :? is a cast

raise (CannotConnectException (Uri("http://google.com"))) //raise is throw

type WithdrawalError =
  | InsufficientFunds of double 
  | WrongPIN
//Result vs Option types
type Result <'Ok,'Error> =
  | Ok of 'Ok 
  | Error of 'Error 

type Option <'Ok> =
  | Some of 'Ok 
  | None 

let result = Error (InsufficientFunds 10.)