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

let flip f a b = f b a  

module Option =
  let getOr defaultValue = function
  | Some x -> x
  | None -> defaultValue

let N = readInt ()
let a =
  readStr().ToCharArray()
  |> Array.map (fun c -> (int c) - (int '1'))

let bm2 n r = n = (r ||| (n-r))

let odd (input: _ array) =
  [0..(N-1)]
  |> List.filter (bm2 (N-1))
  |> List.map (fun i -> input.[i] % 2)
  |> List.fold (^^^) 0
  |> function
  | 0 -> false
  | 1 -> true
  | _ -> failwith "invalid" 

let have1 = Array.exists ((=) 1)
if odd a then 1 else if have1 a then 0 else if a |> Array.map (flip (/) 2) |> odd then 2 else 0
|> printfn "%d"