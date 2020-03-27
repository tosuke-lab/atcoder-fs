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
    // start
     
    let N = readStr().ToCharArray() |> Array.map (int >> (flip (-) (int '0')))
    let K = readInt()
     
    let NL = N.Length
     
    let nth n =
      let rec loop n' i =
        if N.[i] = 0
        then if i < (NL-1) then loop n' (i+1) else None
        else
          if n'=1 then Some i
          elif i < (NL-1) then loop (n'-1) (i+1)
          else None
      loop n 0
     
    let combi n k =
      let pnk = [(n-k+1)..n] |> List.fold ( * ) 1
      let factK = [1..k] |> List.fold ( * ) 1
      pnk / factK
     
    let count n =
      let inner i =
        let v = N.[i]
        let rest = max 0 (NL-i-1)
        let nonZero = (v-1) * (pown 9 (K-n)) * (combi rest (K-n))
        let zero = (pown 9 (K-n+1)) * (combi rest (K-n+1))
        nonZero + zero
      nth n |> Option.map inner |> Option.getOr 0
     
    let tmp = [1..K] |> List.sumBy count
    if Option.isSome (nth K)
    then tmp + 1
    else tmp
    |> printfn "%d"