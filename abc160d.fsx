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
let (N, X, Y) = readInts() |> triple

let len i j =
  let l a b = abs (a-b)
  min ((l i X)+1+(l Y j)) (l i j)

let result =
  let bin = Array.create 3000 0
  for i in 1..(N-1) do
    for j in (i+1)..N do
      Array.modify bin (len i j) inc
  bin

for k in 1..(N-1) do
  printfn "%d" result.[k]