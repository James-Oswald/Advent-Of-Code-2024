
import Std.Data.HashSet
import Std.Data.HashSet

structure LabState where
  dim : Nat × Nat -- Width x Height
  gpos : Int × Int -- Guard position
  gdir : Int × Int -- x,y direction the guard is facing
  objs : Std.HashSet (Int × Int) -- Positions of objects in the lab
deriving Inhabited

instance : ToString LabState where
  toString s := s!"{s.dim} {s.gpos} {s.gdir} {s.objs.toList.toString}"

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
partial def LabState.run (s : LabState) : List ((Int × Int) × (Int × Int)) :=
  let next_pos := (s.gpos.1 + s.gdir.1, s.gpos.2 + s.gdir.2);
  if s.inBounds next_pos then
    if s.objs.contains next_pos then
      {s with gdir := rotate s.gdir}.run
    else
      {s with gpos := next_pos}.run.append [(s.gpos, s.gdir)]
  else
    [(s.gpos, s.gdir)]

def parseInput (s : String) : LabState := Id.run do
  let mut objs := Std.HashSet.empty
  let mut gpos : Int × Int := (0, 0)
  let mut gdir := (0, 0)
  let mut dim := (0, 0)
  for ⟨i, line⟩ in s.splitOn "\n"|>.enum do
    dim := {dim with fst := i}
    for ⟨j, c⟩ in line.data.enum do
      dim := {dim with snd := j}
      match c with
      | '^' | 'v' | '<' | '>' => gpos := (j, i); gdir := rotation c
      | '#' => objs := objs.insert ((j : Int), (i : Int))
      | _ => ()
  return {dim, gpos, gdir, objs}

def part1 (s : String) : Nat :=
  parseInput s |>.run |>.map Prod.fst |> Std.HashSet.ofList |>.toList.length

def get_dupes (s : List ((Int × Int) × (Int × Int))) :
Std.HashMap (Int × Int) (List (Int × Int)) := Id.run do
  let mut d := Std.HashMap.empty
  for (pos, dir) in s do
    d := d.insert pos (if H : d.contains pos then (dir::d[pos]) else [dir])
  return d.filter (λ _ v => v.length > 1)

-- def isLoopPoint (s : List ((Int × Int) × (Int × Int))) (p : (Int × Int)) : Bool :=
--   let mut d := Std.HashMap.empty
--   for (pos, dir) in s do
--     d := d.insert pos (if H : d.contains pos then (dir::d[pos]) else [dir])
--   d.contains p

def part2 (s : String) : Nat :=
  let path := parseInput s |>.run
  dbg_trace s!"{path}"
  let dupes := get_dupes path
  dbg_trace s!"{dupes.toList}"
  s.length

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day6s.txt"
  IO.print s!"Part 1: {part1 input}\n"
  IO.print s!"Part 2: {part2 input}\n"
  pure ()
