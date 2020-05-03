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

let quadro = function
| [|a; b; c; d|] -> (a,b,c,d)
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
let (N,M,Q) = readTriple int int int ()

let abcd = [| for _ in 1..Q -> reads int () |> quadro |]

let rec A n m =
  if n=0 then Seq.singleton []
  else seq {
    for a in m..M do
      yield!
        A (n-1) a
        |> Seq.map (fun l -> a::l)
  }

A N 1
|> Seq.map (List.toArray >> (fun s ->
  let r =
    abcd
    |> Array.fold (fun sum (a,b,c,d) -> sum + if (s.[b-1]-s.[a-1])=c then d else 0) 0
  r
))
|> Seq.max
|> printfn "%d"
