open System
open System.Collections.Generic

// prelude
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int64

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

let inline memo2 f =
  let dp = new Dictionary<_,_>()
  let rec g x y =
    match dp.TryGetValue((x,y)) with
    | true, v -> v
    | false, _ ->
      let v=f g x y
      dp.Add((x,y), v)
      v
  g


// start
let N=readInt()
let A=readInts()

let count = memo2 <| fun count c i ->
  if c=0 then 0L
  elif i<0||(2*c-i)>2 then int64 -1e15
  else
    max (count c (i-1)) (A.[i]+(count (c-1) (i-2)))

count (N/2) (N-1)
|> printfn "%d"