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

let N=readInt()

let xy=[|
  for _ in 1..N ->
    let A=readInt()
    [|
      for _ in 1..A ->
        let (x,y)=readInts()|>pair
        (x-1,y)
    |]
|]

let check n =
  xy
  |> Array.mapi (fun i x->i,x)
  |> Array.forall (fun (i,xy) ->
    if n&&&(1<<<i)=0 then true
    else
      xy
      |> Array.forall (fun (x,y) ->
        let a=n&&&(1<<<x)
        if y=1 then a<>0 else a=0
      )
  )

let rec popcnt n =
  if n=0 then 0
  else
    popcnt (n>>>1)
    + if n&&&1<>0 then 1 else 0

seq {
  for i in 0..((1<<<N)-1) do
    if check i then
      yield i
}
|> Seq.map popcnt
|> Seq.max
|> printfn "%d"