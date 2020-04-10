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
let (H,W) = readInts() |> pair
let S = [| for _ in 1..H -> readStr().ToCharArray() |]

let r =
  let mutable ans = 0
  let seen=Array2D.create H W false
  for i in 0..(H-1) do
    for j in 0..(W-1) do
      let rec dfs b w y x =
        seen.[y,x]<-true
        let (b',w')=if S.[y].[x]='#' then (b+1,w) else (b,w+1)
        [(y+1,x);(y-1,x);(y,x-1);(y,x+1)]
        |> List.fold (fun (b,w) (y',x') ->
          if
            0<=y'&&y'<H&&0<=x'&&x'<W
            && (not seen.[y',x'])
            && S.[y'].[x']<>S.[y].[x]
          then dfs b w y' x'
          else (b,w)
        ) (b',w')
      if S.[i].[j]='#'&& not seen.[i,j] then
        let (b,w)=dfs 0 0 i j
        ans <- ans + b*w
  ans

printfn "%d" r