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
let (H,W) = readInts() |> pair
let s =
  let s=Array2D.zeroCreate H W 
  for i in 0..(H-1) do
    readStr().ToCharArray() |> Array.iteri (fun j x -> s.[i,j]<-x='.')
  s

let sum =
  seq {
    for i in 0..(H-1) do
      for j in 0..(W-1) do
        yield s.[i,j]
  }
  |> Seq.sumBy (function | true -> 1 | false -> 0)

let route =
  let r = Array2D.create H W None
  let rec bfs = function
    | Queue.Nil -> ()
    | Queue.Cons ((y,x), que) ->
      let l = r.[y,x] |> Option.map inc
      [(y-1,x); (y+1,x); (y,x-1); (y,x+1)]
      |> List.fold (fun que (p,q) ->
        if
          0<=p&&p<H&&0<=q&&q<W
          && s.[p,q]
          && Option.isNone r.[p,q]
        then
          r.[p,q]<-l
          que |> Queue.snoc (p,q)
        else
          que
      ) que
      |> bfs

  r.[0,0] <- Some 0
  bfs (Queue.singleton (0,0))
  r.[H-1,W-1]

route
|> function
  | None -> -1
  | Some r -> sum - r - 1
|> printfn "%d"