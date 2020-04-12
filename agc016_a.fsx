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
let s=readStr().ToCharArray() |> Array.toList

let rec shrink c = function
  | [_] -> []
  | _::b::s when b=c -> b::(shrink c (b::s))
  | a::s -> a::(shrink c s)
  | [] -> []

let check c = List.forall ((=) c)

let rec count c s = if check c s then 0 else 1+(count c (shrink c s))

['a'..'z']
|> List.fold (fun st c -> min st (count c s)) 1000
|> printfn "%d"
