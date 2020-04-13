open System

// input
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int

// utils

let pair = function
| [|a; b|] -> (a, b)
| _ -> failwith "owatta"

let triple = function
| [|a; b; c|] -> (a, b, c)
| _ -> failwith "owatta"

let inline flip f a b = f b a  

// memoization
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

let inline memo2 size1 size2 f =
  let dp = Array2D.create size1 size2 None
  let rec g x y =
    match Array2D.get dp x y with
    | None ->
      let v = f g x y
      Array2D.set dp x y (Some v)
      v
    | Some v -> v
  g

module Option =
  let getOr defaultValue = function
  | Some x -> x
  | None -> defaultValue

module Array =
  let modify array i f = Array.set array i (f (Array.get array i))

let bin = Array.create (3 * 100000) 0L

let N = readInt ()
let A = readInts ()

A |>
Array.iter (fun a -> Array.modify bin a ((+) 1L))

let calc n = (n * (n - 1L)) / 2L

let result =
  bin
  |> Array.map calc

let sum = result |> Array.sum

[0..(N-1)]
|> List.map (fun x -> sum - result.[A.[x]] + calc (bin.[A.[x]] - 1L))
|> List.iter (printfn "%d")
