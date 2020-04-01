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
let (N, K) = readInts() |> pair

let T=
  let t=Array2D.zeroCreate N K
  for i in 0..(N-1) do
    readInts()
    |> Array.iteri (fun j -> Array2D.set t i j)
  t

let hasBug=
  let rec go i j=
    let v=T.[i,j]
    if i=N-1 then Seq.singleton v
    else
      seq {
        for j' in 0..(K-1) do
          yield! go (i+1) j'
      }
      |> Seq.map (fun x->x^^^v)
  seq {
    for j in 0..(K-1) do
      yield! go 0 j
  }
  |> Seq.exists ((=) 0)

printfn "%s" (if hasBug then "Found" else "Nothing")
