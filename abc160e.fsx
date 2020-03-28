open System

// prelude
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
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

// start
let XYC = readInts()
let X, Y = XYC.[0], XYC.[1]
let p = readInts()
let q = readInts()
let r = readInts() |> Array.sortDescending

let pTopX = p |> Array.sortDescending |> Array.take (int X)
let qTopY = q |> Array.sortDescending |> Array.take (int Y)

Array.concat [pTopX; qTopY; r]
|> Array.sortDescending
|> Array.take (int (X+Y))
|> Array.sum
|> printfn "%d"
