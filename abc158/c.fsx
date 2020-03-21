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

let (A, B) = readInts () |> pair

let tax per yen = yen * per / 100

seq { 1..10007 }
|> Seq.tryFind (fun y -> tax 8 y = A && tax 10 y = B)
|> Option.getOr -1
|> printfn "%d"