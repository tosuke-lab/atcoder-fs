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

// memoization
let inline memo size f =
  let dp = Array.create size None
  let rec g x =
    match dp.[x] with
    | None ->
      let v = f g x
      dp.[x] <- Some v
      v
    | Some v -> v
  g

let inline memo2 size1 size2 f =
  let dp = Array2D.create size1 size2 None
  let rec g x y =
    match dp.[x, y] with
    | None ->
      let v = f g x y
      dp.[x, y] <- Some v
      v
    | Some v -> v
  g

// SegTree
module SegTree =
  type SegTree<'a>(f: 'a -> 'a -> 'a, zero: 'a, num: int, data: 'a array) =
    static member Create f zero (src: _ array): _ SegTree =
      let num =
        let rec loop n' = if n' < src.Length then loop (n'*2) else n'
        loop 1
      let data = Array.create (2*num-1) zero

      for i in 0..(src.Length-1) do
        data.[i+num-1] <- src.[i]

      let rec loop n =
        for i in (n-1)..(2*n-2) do
          data.[i] <- f data.[2*i+1] data.[2*i+2]
        if n > 1 then do loop (n/2)

      loop (num/2)
      SegTree(f, zero, num, data)

    // [a, b)
    member this.Query a b =
      let rec loop i l r =
        let ave = (l+r)/2
        if r <= a || b <= l then zero
        elif a <= l && r <= b then data.[i]
        else
          f (loop (2*i+1) l ave) (loop (2*i+2) ave r)
      loop 0 0 num

    member private this.Update i =
      let rec loop n j =
        let i' = j + n - 1
        data.[i'] <- f data.[2*i'+1] data.[2*i'+2]
        if n > 1 then do loop (n/2) (j/2)
      loop (num/2) (i/2)

    member this.Item
      with get(i) = data.[i + num - 1]
      and set i v =
        data.[i + num - 1] <- v
        this.Update i
    member this.Data = data
  
  // utils
  let create = SegTree<_>.Create
  /// query in [a, b) (logN)
  let query a b (segTree: SegTree<_>) = segTree.Query a b
  let modify (segTree: SegTree<_>) i f = segTree.[i] <- f segTree.[i]
// SegTree

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
