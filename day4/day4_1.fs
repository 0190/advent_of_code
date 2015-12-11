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

let startsWithFiveZeros (s : string) =
  s.[0..4] = "00000"

let hashStartsWithFiveZeros (v : int, hash : string) =
  startsWithFiveZeros hash

let answerPair =
  strNumbers
  |> Seq.map (fun i -> keyHashPair i)
  |> Seq.filter hashStartsWithFiveZeros
  |> Seq.head

let decodedInt = fst answerPair
