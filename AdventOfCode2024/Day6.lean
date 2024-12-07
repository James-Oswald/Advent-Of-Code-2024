
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
-- O(wh) where wh is the width x height of the lab, worst case scenario guard bounces around the entire lab
-- before we detect a loop or exit.
partial def LabState.run (s : LabState) (v : Std.HashSet GState := {}) : Option (List GState) :=
  let ⟨⟨w, h⟩, gs, os⟩ := s
  let ⟨p,d⟩ := gs
  let np := (p.1 + d.1, p.2 + d.2) -- Next position
  if v.contains gs then                                    -- Guard is stuck in a loop
    none
  else if 0 > np.1 || np.1 > w || 0 > np.2 || np.2 > h then -- Guard is out of bounds
    some [gs]
  else if os.contains np then                              -- Guard has hit an object
    (gs::.) <$> {s with gs.dir := rotate d}.run (v.insert gs)
  else                                                     -- Guard is in open space
    (gs::.) <$> {s with gs.pos := np}.run (v.insert gs)

def parseInput (input : String) : LabState :=
  let rec loop (i j : Nat) (l : LabState): List Char -> LabState
  | [] => {l with dim := (i, j)}
  | h :: t => match h with
    | '^' | 'v' | '<' | '>' => loop (i+1) j {l with gs := ⟨(i, j), rotation h⟩} t
    | '#' => loop (i+1) j {l with objs := l.objs.insert (i, j)} t
    | '\n' => loop 0 (j+1) l t
    | _ => loop (i+1) j l t
  loop 0 0 default input.data

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
