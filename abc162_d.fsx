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
let N =readInt()
let S=readStr().ToCharArray()

let color = function
  | 'R' -> 0
  | 'G' -> 1
  | 'B' -> 2
  | _ -> failwith "arienq"

let count =
  let res=Array.zeroCreate N
  for i in 0..(N-1) do
    let c=[|for _ in 1..3 -> Array.create 4000 0|]
    for j in (1+i)..(N-1) do
      Array.modify c.[color S.[j]] (j-i) inc
    let s=c |> Array.map (Array.sum)
    res.[i] <- (s,c)
  seq {
    for i in 0..(N-1) do
      let ci=color S.[i]
      for j in (1+i)..(N-1) do
        let cj=color S.[j]
        if ci<>cj then
          let ck=3-ci-cj
          let (s,c)=res.[j]
          yield (s.[ck]-c.[ck].[j-i])
  }
  |> Seq.sumBy int64

printfn "%d" count