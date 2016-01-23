open System

let fileName = "day6.txt"
let separators = [|"turn"; "through"; " "|]
let readLines:(string -> Collections.Generic.IEnumerable<string>) = fun filePath -> filePath |> System.IO.File.ReadLines
let side = 1000

type grid = System.Collections.BitArray
type state = grid * grid

let parseCoords (s : string) =
  let list = s.Split(',') |> Array.toList |> List.map (fun x -> int(x))
  (list.[0], list.[1])

let inRange i l c1 c2 =
  (i / l >= snd c1) && (i / l <= snd c2) &&
    (i % l >= fst c1) && (i % l <= fst c2)

let turningOn (g : grid) i =
  g.Set(i, true)
  true

let lightsOn (arr: state) l s1 s2 =
  let c1 = parseCoords s1
  let c2 = parseCoords s2
  Array.iter (fun i -> if inRange i l c1 c2
                       then (if not ((fst arr).Get(i))
                             then (fst arr).Set(i, true)
                             else (snd arr).Set(i, true)))
             [|for i in 0..((fst arr).Count - 1) -> i|]
  arr

let lightsOff (arr: state) l s1 s2 =
  let c1 = parseCoords s1
  let c2 = parseCoords s2
  Array.iter (fun i -> if inRange i l c1 c2
                       then (if (snd arr).Get(i)
                             then (snd arr).Set(i, false)
                             else (fst arr).Set(i, false)))
             [|for i in 0..((fst arr).Count - 1) -> i|]
  arr

let lightsToggle (arr: state) l s1 s2 =
  let c1 = parseCoords s1
  let c2 = parseCoords s2
  Array.iter (fun i -> if inRange i l c1 c2
                       then (if (turningOn (fst arr) i)
                             then (snd arr).Set(i, true)))
             [|for i in 0..((fst arr).Count - 1) -> i|]
  arr

let rec runCommands (s : state) commands =
  match commands with
  | [] -> s
  | [|"on"; c1; c2|] :: tail -> runCommands (lightsOn s side c1 c2) tail
  | [|"off"; c1; c2|] :: tail -> runCommands (lightsOff s side c1 c2) tail
  | [|_; c1; c2|] :: tail -> runCommands (lightsToggle s side c1 c2) tail

let arr = fileName |> readLines |> (Seq.map (fun x -> x.Split(separators, StringSplitOptions.RemoveEmptyEntries))) |> Seq.toList
let result = runCommands (System.Collections.BitArray(side * side), System.Collections.BitArray(side * side)) arr

let onCount (result : System.Collections.BitArray) =
  Array.fold (fun acc i -> if result.Get(i) then acc + 1 else acc) 0 [|for i in 0..(result.Count - 1) -> i|]

let c = onCount (fst result) + onCount (snd result)
















