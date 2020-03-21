open System

// input
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int

// utils

let pair = function
| [|a; b|] -> (a, b)
| _ -> failwith "owatta"

let triple = function
| [|a; b; c|] -> (a, b, c)
| _ -> failwith "owatta"

let flip f a b = f b a  

module Option =
  let getOr defaultValue = function
  | Some x -> x
  | None -> defaultValue

module Str =
  let length str = (str: string).Length
  let startsWith value str = (str: string).StartsWith(value: string)
  let toCharArray (str: string) = str.ToCharArray()
  let reverse (str: string) =
    let chars = str |> toCharArray
    Array.Reverse chars
    String.Concat chars

let S = readStr()

S
|> Str.toCharArray
|> Array.distinct
|> Array.length
|> function
| 2 -> "Yes"
| _ -> "No"
|> printfn "%s"