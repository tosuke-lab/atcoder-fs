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

let inline memo2 size1 size2 f =
  let dp = Array2D.create size1 size2 None
  let rec g x y =
    match dp.[x, y] with
    | None ->
      let v = f g x y
      dp.[x, y] <- Some v
      v
    | Some v -> v
  g

// start
let (H, W) = readInts() |> pair
let c=
  let c=Array2D.zeroCreate 10 10
  for i in 0..9 do
    readInts() |> Array.iteri (fun j -> Array2D.set c i j)
  c
let A=
  seq {
    for _ in 1..H do
      yield! readInts()
  }

let minCost = 
  let d= c |> Array2D.copy
  for k in 0..9 do
    for i in 0..9 do
      for j in 0..9 do
        Array2D.modify d i j (min (d.[i,k]+d.[k,j]))
  Array2D.get d

A
|> Seq.sumBy (fun x -> if x= -1 then 0 else minCost x 1)
|> printfn "%d"