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
let N = read int ()
let A = reads int ()

A
|> Array.sort
|> Array.fold (fun st x ->
  match st with
  | (y,c)::l -> if y=x then (y,c+1)::l else (x,1)::(y,c)::l
  | [] -> [(x,1)]
) []
|> List.map (snd >> (fun n -> ((n-1)%2)+1))
|> List.fold (fun (n,m) x -> if x=1 then (n,m+1) else (n+1,m)) (0,0)
|> fun (n,m) -> m + (n/2)*2
|> printfn "%A" 
