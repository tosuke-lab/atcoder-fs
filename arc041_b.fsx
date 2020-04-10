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
let (N,M)=readInts()|>pair
let b=
  let b=Array2D.create (N+4) (M+2) 0
  for i in 1..N do
    readStr().ToCharArray() |> Array.iteri (fun j c -> b.[i,1+j]<-(int c-int '0'))
  b

let a=Array2D.create (N+2) (M+2) 0
for i in 1..N do
  for j in 1..M do
    let v=b.[i,j]
    a.[i+1,j]<-v
    let m=flip (-) v
    Array2D.modify b i j m
    Array2D.modify b (i+1) (j-1) m
    Array2D.modify b (i+1) (j+1) m
    Array2D.modify b (i+2) j m
    ()

for i in 1..N do
  for j in 1..M do
    printf "%d" a.[i,j]
  printfn ""