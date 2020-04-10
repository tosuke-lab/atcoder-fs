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
let (R,C)=readInts() |> pair
let (sy,sx)=readInts()|>Array.map dec|>pair
let (gy,gx)=readInts()|>Array.map dec|>pair
let c=
  let c=Array2D.zeroCreate R C
  for i in 0..(R-1) do
    readStr().ToCharArray() |> Array.iteri (fun j x -> c.[i,j] <- x='.')
  c

let result =
  let r=Array2D.create R C None
  let rec bfs = function
    | Queue.Nil -> ()
    | Queue.Cons ((y,x), q) ->
      let q'=
        [(y-1,x);(y+1,x);(y,x-1);(y,x+1)]
        |> List.fold(fun q (y',x') ->
          if
            0<=y'&&y'<R&&0<=x'&&x'<C
            && Option.isNone r.[y',x']
            && c.[y',x']
          then
            r.[y',x'] <- r.[y,x] |> Option.map inc
            q |> Queue.snoc (y',x')
          else q
        ) q
      bfs q'
  r.[sy,sx]<-Some 0
  bfs (Queue.singleton (sy,sx))
  r.[gy,gx] |> Option.get

printfn "%d" result