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
let p = readInts()

let sum =
  let s = Array.create N 0
  for i in 0..(N-1) do
    s.[i] <- p.[i]
  for i in 1..(N-1) do
    Array.modify s i ((+) s.[i-1])
  
  let f x1 x2 = s.[x2] - (if x1=0 then 0 else s.[x1-1])
  f

seq {
  for i in 0..(p.Length-K) do
    yield sum i (i+K-1)
}
|> Seq.max
|> (fun p -> (float (K+p))/2.)
|> printfn "%f"
