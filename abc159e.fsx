open System
open System.Numerics

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

let rec popCnt = function
| 0 -> 0
| n -> (if (n &&& 1) <> 0 then 1 else 0) + popCnt (n>>>1)

let main () =
  let (H, W, K) = readInts() |> triple

  let grid =
    let g = Array2D.create H W 0
    for y in 0..(H-1) do
      let s = readStr().ToCharArray()
      for x in 0..(W-1) do
        g.[y, x] <- if s.[x] = '1' then 1 else 0
    g

  seq {
    for split in 0..((1<<<H-1)-1) do
      let sum =
        let s = Array2D.create 10 W 0
        let mutable i = 0
        for y in 0..(H-1) do
          for x in 0..(W-1) do
            Array2D.modify s i x ((+) grid.[y, x])
          if (split &&& (1<<<y)) <> 0 then do i <- i + 1
        for y in 0..(10-1) do
          for x in 1..(W-1) do
            Array2D.modify s y x ((+) s.[y, x-1])
        s
      
      let pred x1 x2 =
        [0..9]
        |> List.forall (fun i ->
          let c = sum.[i, x2] - if x1=0 then 0 else sum.[i, x1-1]
          c <= K
        )
      
      let rec loop x1 x2 =
        let rec eat x = if pred x1 x && x < x2 then eat (x+1) else x
        let x' = eat x1
        if x'=x1 then 10*1000
        elif x'=x2 then 0
        else 1 + loop x' x2

      let cnt = loop 0 (W-1) + popCnt split
      yield cnt
  }
  |> Seq.min
  |> printfn "%A"

main()