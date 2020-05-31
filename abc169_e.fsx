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
let N = read int ()
let AB = [|for _ in 1..N -> readPair int int ()|]

let AB1 = AB |> Array.sortBy fst
let AB2 = AB |> Array.sortBy snd

if N%2 = 1 then
  let i = N/2
  let M = snd AB2.[i]
  let m = fst AB1.[i]
  (M-m+1) |> printfn "%d"
else
  let i =N/2-1
  let j =N/2
  let M = snd AB2.[i] + snd AB2.[j]
  let m = fst AB1.[i] + fst AB1.[j]
  (M-m+1) |> printfn "%d"