open System

// prelude
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int

let pair = function
| [|a; b|] -> (a, b)
| _ -> failwith "owatta"

let triple = function
| [|a; b; c|] -> (a, b, c)
| _ -> failwith "owatta"

let inc n = n + 1
let dec n = n - 1

let inline flip f a b = f b a
let rec fix f = fun x -> (f (fix f)) x

module Option =
  let getOr defaultValue = function
  | Some x -> x
  | None -> defaultValue

module Array =
  let modify (arr: _ []) i f =
    arr.[i] <- f arr.[i]

module Array2D =
  let modify (arr: _ [,]) i j f =
    arr.[i, j] <- f arr.[i, j]

// start
let s=readStr().ToCharArray()

let kaibun =
  let rec inner acc i j =
    if i>=j then acc
    elif s.[i]=s.[j] then inner acc (i+1) (j-1)
    elif s.[i]='x' then inner (acc |> Option.map inc) (i+1) j
    elif s.[j]='x' then inner (acc |> Option.map inc) i (j-1)
    else None
  inner (Some 0) 0 (s.Length-1)
  |> Option.getOr -1

printfn "%d" kaibun