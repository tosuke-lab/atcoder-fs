// Binominal Heap
type Tree<'a when 'a: comparison> = Node of int * 'a * Tree<'a> list
type BinominalHeap<'a when 'a: comparison> = Heap of Tree<'a> list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BinominalHeap =
  let empty = Heap []
  let singleton x = Heap [Node(0, x, [])]
 
  let isEmpty = function
    | Heap [] -> true
    | _ -> false
  let private rank (Node(r,_,_)) = r
  let private root (Node(_,x,_)) = x
  let inline private link (Node(r,x1,c1) as t1) (Node(_,x2,c2) as t2) =
    if x1<=x2 then Node(r+1, x1, t2::c1) else Node(r+1, x2, t1::c2)

  let inline private insTree t h =
    let rec go t h =
      match h with
      | [] -> [t]
      | t1::hs ->
        if rank t < rank t1 then t::h else go (link t t1) hs
    go t h

  let inline merge (Heap h1) (Heap h2) =
    let rec go h1 h2 =
      match (h1, h2) with
      | (_, []) -> h1
      | ([], _) -> h2
      | (t1::h1s, t2::h2s) ->
        let r1 = rank t1
        let r2 = rank t2
        if r1<r2 then t1::(go h1s h2)
        elif r2>r1 then t2::(go h1 h2s)
        else insTree (link t1 t2) (go h1s h2s)
    Heap(go h1 h2)
  
  let inline insert x (Heap h) = Heap(insTree (Node(0, x, [])) h)

  let inline private uncons h =
    let rec go = function
      | [] -> failwith "empty"
      | [t] -> (t, [])
      | t::ts ->
        let (t', ts') = go ts
        if root t<=root t' then (t, ts) else (t', t::ts')
    go h
  
  let inline head (Heap h) = uncons h |> fst |> root
  let inline tail (Heap h) =
    let (Node(_,x,c), hs) = uncons h
    merge (Heap(List.rev c)) (Heap hs)

  let (|Cons|Nil|) (Heap h) =
    if List.isEmpty h then Nil
    else
      let (Node(_,x,c), hs) = uncons h
      Cons(x, (merge (Heap(List.rev c)) (Heap hs)))


let heap = [2; 3; 5; 4; 1] |> List.fold (fun h x -> BinominalHeap.insert x h) BinominalHeap.empty

let list =
  let rec go = function
    | BinominalHeap.Nil -> []
    | BinominalHeap.Cons(x, hs) -> x::(go hs)
  go heap

printfn "%A" list
      