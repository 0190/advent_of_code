#load "command_parser.fs"
open CommandParser

let field = Map<coord,bool> []

let switchSome s (c1, c2) d =
  let lights = lightsInRectangle c1 c2
  let rec helper (l: seq<coord>) (s:state) =
    if l |> Seq.isEmpty then s
    else
      let e = l |> Seq.head
      let t = l |> Seq.tail
      if Map.exists (fun k v -> k = e) s then helper t (if d = "on" then s else (Map.remove e s))
      else helper t (if d = "off" then s else (Map.add e true s))
  helper lights s

let turnOn (s: state) (c1 : coord, c2 : coord) =
  switchSome s (c1, c2) "on"

let turnOff (s: state) (c1 : coord, c2 : coord) =
  switchSome s (c1, c2) "off"

let toggle (s: state) (c1 : coord, c2 : coord) =
  switchSome s (c1, c2) "toggle"

let defaultParseCommand (line : string) =
  parseCommand turnOn turnOff toggle line

let runOnField c f =
  (fst c) f (snd c)

let run filePath parser init =
  let rec helper l f =
    match l with
    | [] -> f
    | e :: l' -> helper l' (runOnField e f)
  helper (filePath |> readLines |> Seq.map parser |> Seq.toList) init

//let answer = run fileName defaultParseCommand field
let answer = run "day6_tests.txt" defaultParseCommand field