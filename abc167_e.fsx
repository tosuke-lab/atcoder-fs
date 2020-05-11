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

// ModInt
type ModInt = MVal of int64

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ModInt =
  let modulo = 998244353L

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
let (N,M,K) = readTriple int int int ()

let rec pow n k =
  if k=0 then ModInt.init 1
  else
    n * (pow n (k-1))

let facts = Array.zeroCreate (N+1)
facts.[0] <- ModInt.init 1
for i in 1..N do
  facts.[i]<-facts.[i-1]*(ModInt.init i)

let invFacts = facts |> Array.map (fun x -> (ModInt.init 1)/x)

let C n k = facts.[n]*invFacts.[n-k]*invFacts.[k] 

let cnt0 = Array.zeroCreate (N+1)
cnt0.[1]<-ModInt.init M
for i in 2..N do
  cnt0.[i]<-cnt0.[i-1]*(ModInt.init (M-1))

let rec cnt n k =
  if k=0 then
    cnt0.[n]
  else
    (C (n-1) k) * (cnt (n-k) 0)

[0..K]
|> List.fold (fun st k -> st + (cnt N k)) (ModInt.init 0)
|> ModInt.value
|> printfn "%d"