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

// start

// https://atcoder.jp/contests/arc011/tasks/arc011_2

let N = readInt()
let ws = readStr().Split()

let f (w: string) =
  let rec inner a =
    function
    | [] ->
      let r =
        a
        |> List.rev
        |> String.concat ""
      if r.Length > 0 then Some r else None
    | h::t ->
      let k =
        match h with
        | 'b'|'c' -> "1"::a
        | 'd'|'w' -> "2"::a
        | 't'|'j' -> "3"::a
        | 'f'|'q' -> "4"::a
        | 'l'|'v' -> "5"::a
        | 's'|'x' -> "6"::a
        | 'p'|'m' -> "7"::a
        | 'h'|'k' -> "8"::a
        | 'n'|'g' -> "9"::a
        | 'z'|'r' -> "0"::a
        | _ -> a
      inner k t
  inner [] (w.ToLower().ToCharArray() |> Array.toList)

sprintf " %s" ""

ws
|> Array.toList
|> List.collect (f >> Option.toList)
|> String.concat " "
|> printfn "%s"
