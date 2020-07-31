namespace Program

open System

// [<EntryPoint>]
[<RequireQualifiedAccess>] //require caller of method to prepend module name before method
module Option =
  let fromTryTuple = function
    | false, _ -> None 
    |true, x -> Some x

[<RequireQualifiedAccess>]
module Result =
  let fromOption errorValue = function 
    | Some x -> Ok x 
    | None -> Error errorValue

module Parser =
  let tryParse (input:string) =
    Int32.TryParse input
    |> Option.fromTryTuple

module Validator =
  type ValidNumber = private ValidNumber of int //private constructor
  module ValidNumber = //appears as static method
    let tryValidNum number =
      match 1 <= number && number <= 4000 with 
      | false -> None 
      | true -> Some  <| ValidNumber number
    let value (ValidNumber number) = number

module Algorithm =
  open Validator

  let getAlgString validNumber =
    [1 .. ValidNumber.value validNumber]
    |> List.map(fun n -> (n, n%3, n%5)
    >> function
    | (_,0,0) -> "FizzBuzz"
    | (_,0,_) -> "Fizz"
    | (_,_,0) -> "Buzz"
    | (n,_,_) -> string n )
    |> String.concat "\n"

module Domain =
  open Validator

  type ParseNumber = string -> int option
  type ValidateNumber = int -> ValidNumber option 
  type GetTheAlgString = ValidNumber -> string

  type ParserError = NotANumber of string //for non-numbers
  type ValidatorError = InvalidNumber of int //for invalid numbers
  //type DomainError = exn //exception
  type DomainError = //error for each domain
    | ParserError of ParserError 
    | ValidatorError of ValidatorError

  type ExecuteAlgorithm = string -> Result<string, DomainError>

  let execute (parseNumber:ParseNumber)(validateNumber:ValidateNumber)
    (getTheAlgString:GetTheAlgString): ExecuteAlgorithm = 
    let parseNumber input =
      input
      |> parseNumber
      |> Result.fromOption (NotANumber input)
      |> Result.mapError ParserError
    
    let validateNumber number =
      number
      |> validateNumber
      |> Result.fromOption (InvalidNumber number)
      |> Result.mapError ValidatorError

    fun input ->
      input
      |> parseNumber
      |> Result.bind validateNumber
      |> Result.map getTheAlgString

module Application = 

  let execute = 
    Domain.execute
      Parser.tryParse
      Validator.ValidNumber.tryValidNum
      Algorithm.getAlgString

