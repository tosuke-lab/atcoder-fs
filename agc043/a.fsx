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

let (H, W) = readInts () |> pair
let table =
  let s = [| for _ in 1..H -> readStr().ToCharArray() |> Array.map (fun c -> c = '.') |]
  Array2D.init H W (fun y x -> s.[y].[x])

let inline memo2 h w f =
  let dp = Array2D.init h w (fun _ _ -> None)
  let rec g y x =
    match Array2D.get dp y x with
    | None ->
      let v = f g y x
      Array2D.set dp y x (Some v)
      v
    | Some v -> v
  g

let isWhite y x = Array2D.get table y x

let calc =
  memo2 104 104 (fun calc' y x ->
    let black = not (isWhite y x)
    let cost y' x' = (if black && isWhite y' x' then 1 else 0) + (calc' y' x') 
    match (y+1 = H), (x+1 = W) with
    | true, true -> if black then 1 else 0
    | true, false -> cost y (x+1)
    | false, true -> cost (y+1) x
    | false, false -> min (cost y (x+1)) (cost (y+1) x)
  )

calc 0 0 |> printfn "%d"