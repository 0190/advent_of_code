#load "command_parser.fs"
open CommandParser

let field = seq<coord> []

let turnOn (s: state) (c1 : coord, c2 : coord) =
  let lights = lightsInRectangle c1 c2
  Seq.concat [s; lights] |> Seq.distinct


let turnOff (s: state) (c1 : coord, c2 : coord) =
  s |> Seq.filter (fun (x,y) ->
    x < fst c1 || x > fst c2 || y < snd c1 || y > snd c2)

let toggle (s: state) (c1 : coord, c2 : coord) =
  let toTurnOff = s |> Seq.filter (fun (x,y) ->
    x >= fst c1 && x <= fst c2 && y >= snd c1 && y <= snd c2)
  // the slow part
  let toTurnOn = (lightsInRectangle c1 c2) |> Seq.filter (fun e -> (not (Seq.exists ((=) e) toTurnOff)))
  // the slow part end
  Seq.concat [turnOff s (c1, c2); toTurnOff]

let defaultParseCommand (line : string) =
  parseCommand turnOn turnOff toggle line // add toggle

let runOnField c f =
  (fst c) f (snd c)

let run filePath parser init =
  let rec helper l f =
    match l with
    | [] -> f
    | e :: l' -> helper l' (runOnField e f)
  helper (filePath |> readLines |> Seq.map parser |> Seq.toList) init

//let answer = run "day6_tests.txt" defaultParseCommand field |> Seq.length
let answer = run fileName defaultParseCommand field |> Seq.length