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
let N = read int64 ()

let rec primes n a =
  if N < a*a then
    if n=1L then [] else [1]
  elif n % a <> 0L then primes n (a+1L)
  else
    let rec inner n acc =
      if n%a<>0L then (n, acc)
      else inner (n/a) (acc+1)
    let (n', c) = inner n 0
    c::(primes n' (a+1L))

let rec cnt n c = if n>=c then 1+(cnt (n-c) (c+1)) else 0

primes N 2L
|> List.fold (
  fun st x -> st + (cnt x 1)
) 0
|> printfn "%d"