
-- Bad brute force solution

structure Calibration where
  test : Nat
  vs : List Nat
deriving Repr

def parseInput (input : String) : List Calibration :=
  input |>.splitOn "\n" |>.map (·.splitOn ":")
  |>.map (λ x => ⟨x[0]!.toNat!, x[1]!.splitOn " " |>.tail.map (·.toNat!)⟩)

def CalibrateWithOps (ops : Nat) : List Nat -> Nat
| [] => 0
| [t] => t
| h::t => dbg_trace s!"Checking {h::t} {ops}"
  if (ops % 2 == 0) then
    Nat.mul h (CalibrateWithOps (ops / 2) t)
  else
    Nat.add h (CalibrateWithOps (ops / 2) t)

def Calibration.check (c : Calibration) : Bool :=
  dbg_trace s!"Checking {c.test} {Nat.pow 2 (c.vs.length - 1)}"
  List.range (Nat.pow 2 (c.vs.length - 1)) |>.any (CalibrateWithOps · c.vs == c.test)

def part1 (input : String) : Nat :=
  parseInput input |>.filter Calibration.check |>.map (·.test) |>.foldl Nat.add 0

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day7s.txt"
  IO.print s!"{part1 input}\n"
  pure ()
