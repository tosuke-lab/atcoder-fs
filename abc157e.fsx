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

// SegTree
module SegTree =
  type SegTree<'a>(f: 'a -> 'a -> 'a, zero: 'a, num: int, data: 'a array) =
    static member Create f zero (src: _ array): _ SegTree =
      let num =
        let rec loop n' = if n' < src.Length then loop (n'*2) else n'
        loop 1
      let data = Array.create (2*num-1) zero

      for i in 0..(src.Length-1) do
        data.[i+num-1] <- src.[i]

      let rec loop n =
        for i in (n-1)..(2*n-2) do
          data.[i] <- f data.[2*i+1] data.[2*i+2]
        if n > 1 then do loop (n/2)

      loop (num/2)
      SegTree(f, zero, num, data)

    // [a, b)
    member this.Query a b =
      let rec loop i l r =
        let ave = (l+r)/2
        if r <= a || b <= l then zero
        elif a <= l && r <= b then data.[i]
        else
          f (loop (2*i+1) l ave) (loop (2*i+2) ave r)
      loop 0 0 num

    member private this.Update i =
      let rec loop n j =
        let i' = j + n - 1
        data.[i'] <- f data.[2*i'+1] data.[2*i'+2]
        if n > 1 then do loop (n/2) (j/2)
      loop (num/2) (i/2)

    member this.Item
      with get(i) = data.[i + num - 1]
      and set i v =
        data.[i + num - 1] <- v
        this.Update i
    member this.Data = data
  
  // utils
  let create = SegTree<_>.Create
  /// query in [a, b) (logN)
  let query a b (segTree: SegTree<_>) = segTree.Query a b
  let modify (segTree: SegTree<_>) i f = segTree.[i] <- f segTree.[i]
// SegTree

let rec popCnt = function | 0 -> 0 | n -> popCnt (n>>>1) + if (n&&&1) = 1 then 1 else 0

let N = readInt ()
let S = readStr ()

let charToSet c = 1<<<(int c - int 'a')

let seg =
  let src = S.ToCharArray() |> Array.map charToSet
  SegTree.create (|||) 0 src

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
    | Modify (i, c) ->  seg.[i-1] <- charToSet c
    | Range (l, r) -> yield (SegTree.query (l-1) r seg |> popCnt)
    
}
|> Seq.map (sprintf "%d")
|> Seq.iter writer.WriteLine 

writer.Dispose()