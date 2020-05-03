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
let (N,M) = readPair int int ()
let H = reads int ()

let g = Array.init N (fun _ -> ResizeArray<int>())
for _ in 1..M do
  let (A,B) = reads int () |> Array.map dec |> pair
  g.[A].Add B
  g.[B].Add A

seq {
  for i in 0..(N-1) do
    let h=H.[i]
    let good =
      g.[i].ToArray()
      |> Array.forall (fun j -> h>H.[j])
    yield if good then 1 else 0
}
|> Seq.sum
|> printfn "%d"
