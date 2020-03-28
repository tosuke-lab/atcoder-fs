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
let N = readInt()
let S = readStr().ToCharArray()

let cost =
  let (esum, wsum) =
    let esum=Array.create N 0
    let wsum=Array.create N 0
    for i in 0..(N-1) do
      Array.modify (if S.[i]='E' then esum else wsum) i inc
    for i in 1..(N-1) do
      Array.modify esum i ((+) esum.[i-1])
      Array.modify wsum i ((+) wsum.[i-1])
    esum, wsum
  let sum (arr: _[]) l r = if l>r then 0 else arr.[r] - if l=0 then 0 else arr.[l-1]
  fun i -> (sum wsum 0 (i-1))+(sum esum (i+1) (N-1))

seq { for i in 0..(N-1) -> cost i }
|> Seq.min
|> printfn "%d"