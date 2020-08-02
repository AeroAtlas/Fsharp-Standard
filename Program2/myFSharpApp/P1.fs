namespace MyFSharpApp
//groupings of code grouped by a name
//can't have lets in namespace
//module static class or grouping of code elements like classes and modules
//modules must be in order to prevent cyclical dependicies 
//order of files from top to bottom also counts when using open command

module Arithmetic = 
  module Addition =
    //let public add (x:int) (y:int) :int = x + y
    let add x y = x + y

module Other =
  open Arithmetic //can also do Arithmetic.Addition.add or open Arithmetic.Addition and then use add function normally

  let program =
    Addition.add 