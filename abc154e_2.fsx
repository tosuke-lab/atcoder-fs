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

let fact = fix <| fun fact n -> if n=0 then 1 else n*fact(n-1)

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

let inline memo2 size1 size2 f =
  let dp = Array2D.create size1 size2 None
  let rec g x y =
    match dp.[x, y] with
    | None ->
      let v = f g x y
      dp.[x, y] <- Some v
      v
    | Some v -> v
  g
// start
let N = readStr().ToCharArray() |> Array.map (int >> (flip (-) (int '0')))
let K = readInt()
let n = N.Length

let count =
  let dp1 = memo2 104 5 <| fun dp1 i k ->
    if k=0 then 1
    elif i>=n then 0
    else 9*(dp1 (i+1) (k-1))+(dp1 (i+1) k)
  let dp = memo2 104 5 <| fun dp i k ->
    if k=0 then 1
    elif i>=n then 0
    else
      let v=N.[i]
      if v=0 then dp (i+1) k
      else (dp (i+1) (k-1))+(max 0 (v-1))*(dp1 (i+1) (k-1))+(dp1 (i+1) k)
  dp 0 K

count |> printfn "%d"