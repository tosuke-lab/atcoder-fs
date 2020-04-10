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
let (N,M,Q)=readInts() |> triple

let query=
  let tbl=Array2D.create (N+2) (N+2) 0
  for _ in 1..M do
    let (l,r)=readInts()|>pair
    // imos
    Array2D.modify tbl 1 r inc
    Array2D.modify tbl 1 (N+1) dec
    Array2D.modify tbl (l+1) r dec
    Array2D.modify tbl (l+1) (N+1) inc
  for i in 1..(N+1) do
    for j in 1..(N+1) do
      Array2D.modify tbl i j ((+) tbl.[i-1,j])
  for i in 1..(N+1) do
    for j in 1..(N+1) do
      Array2D.modify tbl i j ((+) tbl.[i,j-1])
  fun p q -> tbl.[p,q]

for _ in 1..Q do
  let (p,q) = readInts() |> pair
  query p q |> printfn "%d"