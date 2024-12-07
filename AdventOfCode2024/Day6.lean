
import Std.Data.HashSet
import Std.Data.HashSet

structure GState where
  pos : Int × Int -- Guard position
  dir : Int × Int -- x,y direction the guard is facing
deriving Inhabited, BEq, Hashable

structure LabState where
  dim : Nat × Nat -- Width x Height
  gs : GState
  objs : Std.HashSet (Int × Int)
deriving Inhabited

def rotate : Int × Int -> Int × Int
| (1, 0) => (0, 1)
| (0, 1) => (-1, 0)
| (-1, 0) => (0, -1)
| (0, -1) => (1, 0)
| _ => (0, 0)

def rotation : Char -> Int × Int
| '^' => (0, -1)
| 'v' => (0, 1)
| '<' => (-1, 0)
| '>' => (1, 0)
| _ => (0, 0)

-- Play out a lab state, returning none if the guard is stuck in a loop, or the path taken otherwise
partial def LabState.run (s : LabState) (v : Std.HashSet GState := {}) :
Option (List GState) :=
  let ⟨⟨w, h⟩, ⟨p, d⟩, os⟩ := s
  let np := (p.1 + d.1, p.2 + d.2)
  if v.contains s.gs then
    none
  else if !(0 <= p.1 && p.1 <= w && 0 <= p.2 && p.2 <= h) then
    some [s.gs]
  else if os.contains np then
    {s with gs.dir := rotate s.gs.dir}.run (v.insert s.gs) |>.map (s.gs::.)
  else
    {s with gs.pos := np}.run (v.insert s.gs) |>.map (s.gs::.)

def parseInput (s : String) : LabState := Id.run do
  let mut objs := Std.HashSet.empty
  let mut gs := ⟨(0, 0), (0, 0)⟩
  let mut dim := (0, 0)
  for ⟨i, line⟩ in s.splitOn "\n"|>.enum do
    dim := {dim with fst := i}
    for ⟨j, c⟩ in line.data.enum do
      dim := {dim with snd := j}
      match c with
      | '^' | 'v' | '<' | '>' => gs := ⟨(j, i), rotation c⟩
      | '#' => objs := objs.insert ((j : Int), (i : Int))
      | _ => ()
  return {dim, gs, objs}

def uniquePathPositions (s : LabState) : List (Int × Int) :=
   s.run.get!.map GState.pos |> Std.HashSet.ofList |>.toList

def part1 (s : String) : Nat :=
  parseInput s |> uniquePathPositions |>.length

def get_dupes (s : List ((Int × Int) × (Int × Int))) :
Std.HashMap (Int × Int) (List (Int × Int)) := Id.run do
  let mut d := Std.HashMap.empty
  for (pos, dir) in s do
    d := d.insert pos (if H : d.contains pos then (dir::d[pos]) else [dir])
  return d.filter (λ _ v => v.length > 1)

def part2 (s : String) : Nat :=
  let lab := parseInput s
  uniquePathPositions lab
  |>.map (λ p => {lab with objs := lab.objs.insert p})
  |>.filter (·.run.isNone)
  |>.length

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day6.txt"
  IO.print s!"Part 1: {part1 input}\n"
  IO.print s!"Part 2: {part2 input}\n"
  pure ()
