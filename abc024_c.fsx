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
let (N,D,K)=readInts()|>triple
let LR=[|for _ in 1..D -> readInts()|>pair|]
let ST=[|for _ in 1..K -> readInts()|>pair|]

seq {
  for i in 0..(K-1) do
    let (S,T)=ST.[i]
    let rec go p d =
      let (l,r)=LR.[d]
      if p<l||r<p then go p (1+d)
      elif l<=T&&T<=r then 1+d
      elif T<l then go l (1+d)
      else go r (1+d)
    yield go S 0
}
|> Seq.iter (printfn "%d")