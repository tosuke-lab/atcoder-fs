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
  let allPairs (a: _[]) (b: _[]) =
    let r=Array.zeroCreate(a.Length*b.Length)
    for i in 0..(a.Length-1) do
      for j in 0..(b.Length-1) do
        r.[i*b.Length+j] <- (a.[i], a.[j])
    r

module Array2D =
  let modify (arr: _ [,]) i j f =
    arr.[i, j] <- f arr.[i, j]

// start
let N = readInt()
let c = readStr().ToCharArray() |> Array.toList

let ptns =
  let s="ABXY".ToCharArray()
  Array.allPairs s s

let replace (p1, p2) c =
  let rec go = function
    | c1::c2::s when c1=p1&&c2=p2 -> c::(go s)
    | c1::s -> c1::(go s)
    | [] -> []
  go

seq {
  for i in 0..(ptns.Length-1) do
    for j in (i+1)..(ptns.Length-1) do
      yield
        c
        |> replace (ptns.[i]) 'L'
        |> replace (ptns.[j]) 'R'
        |> List.length
}
|> Seq.min
|> printfn "%d"