open System

// prelude
let readStr () = stdin.ReadLine()

let reads f () = readStr().Split() |> Array.map f

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
let (N,M)=readInts()|>pair
let A=reads int64 () |> Array.mapi (fun i x -> x,i) |> Set.ofArray

seq {1..M}
|> Seq.fold (fun set _ ->
  let ((x,i) as m)=Set.maxElement set
  set
  |> Set.remove m
  |> Set.add (x/2L,i)
) A
|> Set.fold (fun s (x,_) -> s+x) 0L
|> printfn "%d"