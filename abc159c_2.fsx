open System

let L = stdin.ReadLine() |> float

let V a b = a * b * (L-a-b)

// 三分探索
// n: 反復回数
// left, right: 下端, 上端
let ternarySearch f n left right =
  let folder (l, r) _ =
    let t1 = (2.*l + r)/3.
    let t2 = (l + 2.*r)/3.
    if f t1 <= f t2 then (t1, r) else (l, t2)
  let (l, r) = [1..n] |> List.fold folder (left, right)
  let x = (l + r) / 2.
  (x, f x)

let result =
  let f a = snd (ternarySearch (V a) 100 0. (L-a))
  snd (ternarySearch f 100 0. L)

result |> printfn "%f"
