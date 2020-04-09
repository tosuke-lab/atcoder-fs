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
let (N,M)=readInts() |> pair
let (X,Y)=readInts() |> pair
let a=readInts() |> Array.toList
let b=readInts() |> Array.toList

let rec A a b t =
  match a with
  | x::a' -> if x>=t then B a' b (x+X) else A a' b t
  | [] -> 0
and B a b t =
  match b with
  | x::b' -> if x>=t then 1+(A a b' (x+Y)) else B a b' t
  | [] -> 0

A a b 0
|> printfn "%d"