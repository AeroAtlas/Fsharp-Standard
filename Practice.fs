let myOne = 1
let hello = "hello"
let letterA = "a"
let isEnabled = true

//function(x + function(y)) or int -> (int -> int)
let add x y = 
  x + y
  //
let add''' (x:string) (y:string) : string = 
  x + y 

//Func<string, int, string> = takes string and int and returns int
let add' = fun x y -> x + y + 4

let add'' x = fun y -> x + y //currying (all functions are curried)

let add5 x =
  let five = 5
  x + 5

let add5' = add 5

add5' 6

// (2 * (number + 3)) ^ 2

let operation number = (2. * (number + 3.)) ** 2.

let add3 = (+) 3
let times2 = (*) 2
let pow2 num = ( ** ) 2. num //num to the 2nd power

//let convertFloat (num:int) = num |> float //or (float num)


let operator'''' number =
  number 
  |>add3
  |>times2
  |>float //converts to float
  |>pow2

let operator' number =
  pow2(float(times2(add3 number)))

let operator'' number =
  let x = add3 number 
  let y = times2 x 
  let z = float y
  pow2 z

let operator number = //Pipeline
  number //equal to number |> add3 |> times2 |> pow2
  |>add3
  |>times2
  |>float
  |>pow2

let operaton'' =  //composition operator
  add3 
  >> times2
  >> float
  >> pow2
// let add3 number = number + 3
// // 3 + number  (right most is first)
// let add3' (number:int) :int = (+) number 3 //prefix operator
// let add3'' = (+) 3 //can remove argument and make it just a 3 plus a' operator
// let divide3 = (/) 3 //this would be a 3 / a' operator