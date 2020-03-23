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

let rec popCnt = function | 0 -> 0 | n -> popCnt (n>>>1) + if (n&&&1) = 1 then 1 else 0

let N = readInt ()
let S = readStr ()

let charToSet c = 1<<<(int c - int 'a')

let (data, bucket, num) =
  let src = S.ToCharArray() |> Array.map charToSet
  let num =
    let rec f n = if n*n < src.Length then f (n+1) else n
    f 1
  let data = Array.create (num*num) 0
  let bucket = Array.create num 0
  for i in 0..(src.Length-1) do
    data.[i] <- src.[i]
    Array.modify bucket (i/num) ((|||) src.[i])
  data, bucket, num

let modify i c =
  data.[i] <- charToSet c
  bucket.[i/num] <-
    let rec f k = if (k/num) = (i/num) then data.[k] ||| f(k+1) else 0
    f ((i/num)*num)

let range a b =
  seq {
    let mutable x = a
    while (x%num)<>0 && x<b do
      yield data.[x]
      x<-x+1
    while b-x>=num do
      yield bucket.[x/num]
      x<-x+num
    while x<b do
      yield data.[x]
      x<-x+1
  }
  |> Seq.fold (|||) 0

type Query = Modify of int * Char | Range of int * int
let readQuery () =
  let qs = readStr().Split()
  match Array.head qs with
  | "1" -> Modify (int qs.[1], qs.[2].ToCharArray().[0])
  | "2" -> Range (int qs.[1], int qs.[2])
  | _ -> failwith "okasii"

let Q = readInt ()

let writer = new IO.StreamWriter(new IO.BufferedStream(Console.OpenStandardOutput()))
seq {
  for _ in 1..Q do
    let q = readQuery ()
    match q with
    | Modify (i, c) -> modify (i-1) c
    | Range (l, r) -> yield (range (l-1) r |> popCnt)
}
|> Seq.map (sprintf "%d")
|> Seq.iter writer.WriteLine 

writer.Dispose()