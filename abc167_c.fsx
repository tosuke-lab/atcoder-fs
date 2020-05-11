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
let (N,M,X) = readTriple int int int ()
let C = Array.zeroCreate N
let A = Array2D.zeroCreate N M
for i in 0..(N-1) do
  let cin = reads int ()
  C.[i] <- cin.[0]
  Array.tail cin |> Array.iteri (fun j x -> A.[i,j]<-x)

seq {
  for n in 0..((1<<<N)-1) do
    let mutable cost = 0
    let st = Array.create M 0
    for i in 0..(N-1) do
      if (n &&& (1<<<i))<>0 then do
        cost <- cost + C.[i]
        for j in 0..(M-1) do
          st.[j]<-st.[j]+A.[i,j]
    if
      st |> Array.forall (fun x -> x>=X)
    then
      yield cost
}
|> Seq.fold (
  fun st c ->
    match st with
    | Some v -> Some (min v c)
    | None -> Some c
) None
|> function
| Some v -> v
| None -> -1
|> printfn "%d"
