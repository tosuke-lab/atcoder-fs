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
  let check (Queue(f, r)) =
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
let (H, W) = readInts() |> pair
let S =
  let s=Array2D.create H W false
  for y in 0..(H-1) do
    readStr().ToCharArray()
    |> Array.iteri (fun x c -> s.[y, x] <- c='.')
  s

let maxRoute (y, x) =
  let g = Array2D.create H W (-1)
  g.[y, x]<-0
  let rec bfs =
    function
    | Queue.Nil -> ()
    | Queue.Cons((i, j), q) ->
      let q'=
        [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
        |> List.fold (fun q' (a, b) ->
          if 0<=a&&a<H && 0<=b&&b<W && S.[a,b] && g.[a,b]<0 then
            g.[a,b]<-g.[i,j]+1
            q' |> Queue.snoc (a,b)
          else q'
        ) q
      bfs q'
  bfs (Queue.ofList [(y, x)])
  let mutable m = 0
  g |> Array2D.iter (fun v-> m<-max m v)
  m

seq {
  for y in 0..(H-1) do
    for x in 0..(W-1) do
      yield (y, x)
}
|> Seq.filter (fun (y,x) -> Array2D.get S y x)
|> Seq.map maxRoute
|> Seq.max
|> printfn "%d"