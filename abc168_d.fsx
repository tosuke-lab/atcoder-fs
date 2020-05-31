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

// Queue
type Queue<'a> = Queue of ('a list * 'a list)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
  let empty = Queue([], [])
  let ofList l = Queue(l, [])
  let singleton x = Queue([x], [])
  let private check (Queue(f, r)) =
    match (f, r) with
    | ([], _) -> Queue(List.rev r, [])
    | _ -> Queue(f, r)
  let snoc x (Queue(f, r)) = Queue(f, x::r) |> check
  let head (Queue(f, _)) = List.head f
  let tryHead (Queue(f, _)) = List.tryHead f
  let tail (Queue(f, r)) =
    match f with
    | [] -> failwith "queue is empty"
    | _::f -> Queue(f, r) |> check
  let tryUncons (Queue(f, r)) =
    match f with
    | [] -> None
    | x::f -> Some(x, Queue(f, r) |> check)
  let (|Cons|Nil|) q =
    match tryUncons q with
    | Some(h,t) -> Cons(h, t)
    | None -> Nil
// Queue

// start
let (N,M) = readPair int int ()
let g = Array.init N (fun _ -> ResizeArray<int>())
for _ in 1..M do
  let (A,B) = readPair int int ()
  g.[A-1].Add (B-1)
  g.[B-1].Add (A-1)

let route =
  let r = Array.create N (-1)
  let rec bfs = function
    | Queue.Nil -> ()
    | Queue.Cons (x, que) ->
      g.[x]
      |> Seq.fold (
        fun q y ->
          if r.[y]>=0 then q
          else
            r.[y]<-x
            q |> Queue.snoc y
      ) que
      |> bfs
  bfs (Queue.singleton 0)
  r

printfn "Yes"
for i in 1..(N-1) do
  printfn "%d" (1+route.[i])