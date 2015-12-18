module Shared
  let key = "yzbqklnj"

  let noTrailingZeros (i : int) =
    i % 10 <> 0

  let strNumbers =
    Seq.initInfinite (fun i -> i)
    |> Seq.filter noTrailingZeros

  let MD5Hash (input : string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

  let keyHashPair (i : int) =
    (i, MD5Hash (key + string i))