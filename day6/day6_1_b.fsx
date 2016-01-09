open System

let fileName = "day6.txt"
let separators = [|"turn"; "through"; " "|]
let readLines:(string -> Collections.Generic.IEnumerable<string>) = fun filePath -> filePath |> System.IO.File.ReadLines

type state = System.Collections.BitArray

let parseCoords (s : string) =
  let list = s.Split(',') |> Array.toList |> List.map (fun x -> int(x))
  (list.[0], list.[1])

let inRange i l c1 c2 =
  (i / l >= snd c1) && (i / l <= snd c2) &&
    (i % l >= fst c1) && (i % l <= fst c2)

let lightsOn (arr: System.Collections.BitArray) l c1 c2 =
  Array.iter (fun i -> if inRange i l c1 c2 then arr.Set(i, true)) [|for i in 0..(arr.Count - 1) -> i|]
  arr

let lightsOff (arr: System.Collections.BitArray) l c1 c2 =
  Array.iter (fun i -> if inRange i l c1 c2 then arr.Set(i, false)) [|for i in 0..(arr.Count - 1) -> i|]
  arr

let lightsToggle (arr: System.Collections.BitArray) l c1 c2 =
  Array.iter (fun i -> if inRange i l c1 c2 then arr.Set(i, (not arr.[i]))) [|for i in 0..(arr.Count - 1) -> i|]
  arr

let rec runCommands (s : state) commands =
  match commands with
  | [] -> s
  | [|"on"; c1; c2|] :: tail -> runCommands (lightsOn s 1000 (parseCoords c1) (parseCoords c2)) tail
  | [|"off"; c1; c2|] :: tail -> runCommands (lightsOff s 1000 (parseCoords c1) (parseCoords c2)) tail
  | [|_; c1; c2|] :: tail -> runCommands (lightsToggle s 1000 (parseCoords c1) (parseCoords c2)) tail

let arr = fileName |> readLines |> (Seq.map (fun x -> x.Split(separators, StringSplitOptions.RemoveEmptyEntries))) |> Seq.toList
let result = runCommands (System.Collections.BitArray(1000000)) arr

let onCount (result : System.Collections.BitArray) =
  Array.fold (fun acc i -> if result.Get(i) then acc + 1 else acc) 0 [|for i in 0..(result.Count - 1) -> i|]

let c = onCount result