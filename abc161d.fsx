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
let K = readInt()

let listup = fix <| fun listup r n ->
  if r=1 then Seq.singleton [n]
  else
    seq {
      if n<>0 then
        yield! (listup (r-1) (n-1))
      yield! (listup (r-1) n)
      if n<>9 then
        yield! (listup (r-1) (n+1))
    }
    |> Seq.map (fun l -> n::l)
  
seq {
  for r in 1..1000 do
    for n in 1..9 do
      yield! (listup r n)
}
|> Seq.take K
|> Seq.last
|> List.fold (fun s x -> s+(string x)) ""
|> printfn "%s"
