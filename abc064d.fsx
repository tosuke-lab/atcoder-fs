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
let N = readInt()
let Ss = readStr()
let S = Ss.ToCharArray()

let rec left s i =
  if i>=N then 0
  elif S.[i]='(' then left (s+1) (i+1)
  else if s=0 then 1+left s (i+1) else left (s-1) (i+1)

let rec right s i =
  if i<0 then 0
  elif S.[i]=')' then right (s+1) (i-1)
  else if s=0 then 1+right s (i-1) else right (s-1) (i-1)

let l=left 0 0
let r=right 0 (N-1)

("(" |> String.replicate l)+Ss+(")" |> String.replicate r)
|> printfn "%s"