open System

// Pairing Heap
type Heap<'a when 'a: comparison> = E | T of 'a * Heap<'a> list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Heap =
  let empty = E
  let isEmpty = function
    | E _ -> true
    | _ -> false
  let merge h1 h2 =
    match (h1, h2) with
    | (E _, _) -> h2
    | (_, E _) -> h1
    | (T(x1,hs1), T(x2,hs2)) ->
      if x1<x2 then T(x1, h2::hs1) else T(x2, h1::hs2)
  let insert x h = merge h (T(x, []))
  let rec private unify = function
    | [] -> E
    | [h] -> h
    | x::y::hs -> merge (merge x y) (unify hs)
  let head = function
    | E _ -> failwith "empty"
    | T(x,_) -> x
  let tail = function
    | E -> E
    | T(_,hs) -> unify hs
  let rec length = function
    | E -> 0
    | T(_, hs) -> 1+(hs |> List.sumBy length)
  let (|Cons|Nil|) = function
    | E -> Nil
    | T(x,hs) -> Cons(x, unify hs)
  
let heap = [2; 3; 5; 1; 4] |> List.fold (fun h x -> Heap.insert x h) Heap.empty
printfn "%A" heap

let list =
  let rec go = function
    | Heap.Nil -> []
    | Heap.Cons(x, h) ->
      printfn "%A" h
      x::(go h)
  go heap