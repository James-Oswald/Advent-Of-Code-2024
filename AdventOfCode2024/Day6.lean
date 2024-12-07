
import Std.Data.HashSet
import Std.Data.HashSet

structure GState where
  pos : Int × Int -- Guard position
  dir : Int × Int -- x,y direction the guard is facing
deriving Inhabited, BEq, Hashable

def GState.nextPos (s : GState) : Int × Int := (s.pos.1 + s.dir.1, s.pos.2 + s.dir.2)

structure LabState where
  dim : Nat × Nat -- Width x Height
  gs : GState
  objs : Std.HashSet (Int × Int) -- Positions of objects in the lab
deriving Inhabited

-- instance : ToString LabState where
--   toString s := s!"{s.dim} {s.gs.pos} {s.gs.dir} {s.objs.toList.toString}"

-- (1, 0) -> (0, 1) -> (-1, 0) -> (0, -1)
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

def LabState.inBounds (s : LabState) (p : Int × Int) : Bool :=
  0 <= p.1 && p.1 <= s.dim.1 && 0 <= p.2 && p.2 <= s.dim.2

-- Play out the lab state getting the path of the guard with directions
partial def LabState.run (s : LabState) : List GState :=
  let next_pos := s.gs.nextPos
  if s.inBounds next_pos then
    if s.objs.contains next_pos then
      {s with gs.dir := rotate s.gs.dir}.run
    else
      s.gs::{s with gs.pos := next_pos}.run
  else
    [s.gs]

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
   s.run.map GState.pos |> Std.HashSet.ofList |>.toList

def part1 (s : String) : Nat :=
  parseInput s |> uniquePathPositions |>.length

def get_dupes (s : List ((Int × Int) × (Int × Int))) :
Std.HashMap (Int × Int) (List (Int × Int)) := Id.run do
  let mut d := Std.HashMap.empty
  for (pos, dir) in s do
    d := d.insert pos (if H : d.contains pos then (dir::d[pos]) else [dir])
  return d.filter (λ _ v => v.length > 1)

-- Play out a lab state, returning true if the guard enters a loop but false if it exits the lab
partial def LabState.loops (s : LabState) (v : Std.HashSet GState := {}) : Bool :=
  if !v.contains s.gs then
    let next_pos := s.gs.nextPos
    if s.inBounds next_pos then
      if s.objs.contains next_pos then
          {s with gs.dir := rotate s.gs.dir}.loops (v.insert s.gs)
      else
        {s with gs.pos := next_pos}.loops (v.insert s.gs)
    else
      false
  else
    true

def part2 (s : String) : Nat :=
  let lab := parseInput s
  uniquePathPositions lab
  |>.map (λ p => {lab with objs := lab.objs.insert p})
  |>.filter LabState.loops
  |>.length

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day6.txt"
  IO.print s!"Part 1: {part1 input}\n"
  IO.print s!"Part 2: {part2 input}\n"
  pure ()
