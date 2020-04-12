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

let gcd = memo2 300 300 <| fun gcd a b ->
  if b=0 then a
  elif a<b then gcd b a
  else gcd b (a%b)

let K =readInt()

seq {
  for a in 1..K do
    for b in 1..K do
      for c in 1..K do
        yield int64 (gcd a (gcd b c))
}
|> Seq.sum
|> printfn"%d"
