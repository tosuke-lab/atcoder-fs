open System

// input
let readStr () = stdin.ReadLine()

let readInt () = stdin.ReadLine() |> int
let readInts () = stdin.ReadLine().Split() |> Array.map int

// utils

let pair = function
| [|a; b|] -> (a, b)
| _ -> failwith "owatta"

let triple = function
| [|a; b; c|] -> (a, b, c)
| _ -> failwith "owatta"

let flip f a b = f b a

let (|Nil|Cons|) list =
  match List.tryHead list with
  | None -> Nil 
  | Some head -> Cons (head, List.tail list)

module Option =
  let getOr defaultValue = function
  | Some x -> x
  | None -> defaultValue

type Query =
  | Reverse
  | PushFront of string
  | PushBack of string

let readQuery () =
  readStr().Split()
  |> Array.toList
  |> function
  | [T] when int T = 1 -> Reverse
  | [T; F; C] when int T = 2 -> if int F = 1 then PushFront C else PushBack C
  | _ -> failwith "owatta"

type State = State of string list * string list

let S = readStr()
let Q = readInt()
let Query = [for _ in 1..Q -> readQuery ()]

let initialState = State (S.Split() |> Array.toList, [])

let reverse (str: string) =
  let chars = str.ToCharArray()
  Array.Reverse chars
  String.Concat chars

Query
|> List.fold (fun (rev, state) query ->
  let pushFront c (State(f, rb)) = State(c::f, rb)
  let pushBack c (State(f, rb)) = State(f, c::rb)
  let rec f = function
  | Reverse -> not rev, state
  | PushFront c -> if not rev then rev, pushFront c state else rev, pushBack c state
  | PushBack c -> if not rev then rev, pushBack c state else rev, pushFront c state
  f query
) (false, initialState)
|> (fun (rev, State (front, rBack)) ->
  let raw = (String.Concat front) + reverse (String.Concat rBack)
  if rev then reverse raw else raw
)
|> printfn "%s"