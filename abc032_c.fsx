open System

// prelude
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int64
let readInts () = stdin.ReadLine().Split() |> Array.map int64

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

let inline memo size f =
  let dp = Array.create size None
  let rec g x =
    match dp.[x] with
    | None ->
      let v = f g x
      dp.[x] <- Some v
      v
    | Some v -> v
  g


// start
let (N,K) =
  let (n,k) = readStr().Split() |> pair
  (int n, int64 k)
let s = [| for _ in 1..(int N) -> readInt() |]

let rec count n i j =
  if j>=N then 0
  elif i>j then count 1L i i
  elif (n*s.[j])<=K then max (j-i+1) (count (n*s.[j]) i (j+1))
  else count (n/s.[i]) (i+1) j

if s |> Array.exists ((=) 0L) then N else count 1L 0 0
|> printfn "%d"