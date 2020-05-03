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

let inc n = n + 1L
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
let S = (read string ()).ToCharArray() |> Array.map (fun c -> int c - int '0')
let N = S.Length

let count = memo2 N 2019 <| fun count j k ->
  let m=S.[j]
  let n=(202*(k-m+2019))%2019
  if j>0 then count (j-1) n else 0L
  + if m=k then 1L else 0L

seq {
  for i in 0..(N-1) -> count i 0
}
|> Seq.sum
|> printfn "%d"