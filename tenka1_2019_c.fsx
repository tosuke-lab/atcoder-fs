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
let N=readInt()
let S=readStr().ToCharArray()

let sumW =
  let s=
    S
    |> Array.scan (fun sum c -> sum + if c='.' then 1 else 0) 0
  fun i j -> s.[j] - s.[i]

let sumB=
  let s=
    S |> Array.scan (fun sum c -> sum + if c='#' then 1 else 0) 0
  fun i j -> s.[j] - s.[i]

seq { for i in 0..N -> (sumB 0 i)+(sumW i N) }
|> Seq.min
|> printfn "%d"