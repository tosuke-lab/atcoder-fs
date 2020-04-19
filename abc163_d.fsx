open System

// prelude
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int

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
// ModInt
type ModInt = MVal of int64

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ModInt =
  let modulo = (int64 1e9)+7L

  let inline init (x: ^a): ModInt =
    let x = (int64 x) % modulo
    match x with
    | _ when x < 0L -> MVal (x + modulo)
    | _ -> MVal x
  
  let value (MVal x) = x
  
type ModInt with
  static member inline ( + ) (lhs: ModInt, rhs: ModInt) =
    let (MVal x), (MVal y) = (lhs, rhs)
    ModInt.init (x+y)
  static member inline ( - ) (lhs: ModInt, rhs: ModInt) =
    let (MVal x), (MVal y) = (lhs, rhs)
    ModInt.init (x-y)
  static member inline ( * ) (lhs: ModInt, rhs: ModInt) =
    let (MVal x), (MVal y) = (lhs, rhs)
    ModInt.init (x*y)
  static member inline ( / ) (lhs: ModInt, rhs: ModInt) =
    let rec pow n m = (if (m&&&1L)=1L then n else ModInt.init 1) * (if m=0L then ModInt.init 1 else (pow (n*n) (m>>>1)))
    let invB = pow rhs (ModInt.modulo-2L)
    lhs * invB
// ModInt

// start
let (N,K) = readPair int64 int64 ()

let sum l r = (l+r)*(r-l+1L)/2L

let min k = sum 0L (k-1L)
let max k = sum (N-k+1L) N

seq {
  for k in K..(N+1L) -> (max k)-(min k)+1L
}
|> Seq.map ModInt.init
|> Seq.fold (+) (ModInt.init 0)
|> ModInt.value
|> printfn "%d"