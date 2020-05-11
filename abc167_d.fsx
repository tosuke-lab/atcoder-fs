open System

// prelude
let readStr () = stdin.ReadLine()

let read f () = readStr() |> f
let reads f () = readStr().Split() |> Array.map f

let pair = function
| [|a; b|] -> (a, b)
| _ -> failwith "owatta"

let triple = function
| [|a; b; c|] -> (a, b, c)
| _ -> failwith "owatta"

let readPair f g () =
  let (a,b) = readStr().Split() |> pair
  (f a, g b)

let readTriple f g h () =
  let (a,b,c) = readStr().Split() |> triple
  (f a, g b, h c)

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
let (N,K) = readPair int int64 ()
let A = reads int () |> Array.map dec

let cnt =
  let rec greedy i k =
    if k=0L then i else greedy A.[i] (k-1L)
  let rec inner map i k =
    if k=0L then i
    elif Map.containsKey i map then
      let d = (Map.find i map) - k
      greedy i (k%d)
    else
      let map' =
        map
        |> Map.add i k
      inner map' A.[i] (k-1L)
  fun i k -> inner Map.empty i k

cnt 0 K |> inc |> printfn "%d"