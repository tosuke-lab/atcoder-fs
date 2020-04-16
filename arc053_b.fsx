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
let S=readStr().ToCharArray()

let bin = 
  let bin = Array.create 26 0
  S |> Array.iter (fun c -> Array.modify bin (int c-int 'a') inc)
  bin

let oddCnt=bin |> Array.sumBy (fun x -> if x%2=1 then 1 else 0)
let N=S.Length

if oddCnt=0 then N
else 1+2*((N-oddCnt)/(2*oddCnt))
|> printfn "%d"