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

let (N, A, B) = readStr().Split() |> Array.map int64 |> triple

let count n = (n / (A+B)) * A + min (n % (A+B)) A

N |> count |> printfn "%d"