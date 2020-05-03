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
let N=read int ()
let SR=[| for _ in 1..N -> readPair string int ()|]

let trues = SR |> Array.filter (fun (s,_) -> s="True") |> Array.map snd |> Array.sortDescending
let falses = SR  |> Array.filter (fun (s,_) -> s="False") |> Array.map snd |> Array.sortDescending

let topTrue = Array.tryHead trues |> Option.getOr -1
let topFalse = Array.tryHead falses |> Option.getOr -1

let res =
  if topTrue>topFalse then "True"
  elif topTrue<topFalse then "False"
  else
    let topTrueNum = trues |> Array.sumBy (fun r -> if r=topTrue then 1 else 0)
    let topFalseNum = falses |> Array.sumBy (fun r -> if r=topTrue then 1 else 0)
    if topTrueNum>topFalseNum then "True"
    elif topTrueNum<topFalseNum then "False"
    else "Draw"

printfn "%s" res