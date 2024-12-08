
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
| h::t =>
  if (ops % 2 == 0) then
    Nat.mul h (CalibrateWithOps (ops / 2) t)
  else
    Nat.add h (CalibrateWithOps (ops / 2) t)

def Calibration.check (c : Calibration) : Bool :=
  List.range (Nat.pow 2 (c.vs.length - 1)) |>.any (λx =>
    CalibrateWithOps x c.vs.reverse == c.test)

def part1 (input : String) : Nat :=
  parseInput input |>.filter Calibration.check |>.map (·.test) |>.foldl Nat.add 0


def CalibrateWithOps2 (ops : Nat) : List Nat -> Nat
| [] => 0
| [t] => t
| h::t =>
  if (ops % 3 == 0) then
    h * (CalibrateWithOps2 (ops / 2) t)
  else if (ops % 2 == 0) then
    h + (CalibrateWithOps2 (ops / 2) t)
  else
    let ed := (CalibrateWithOps2 (ops / 2) t)
    let len := ed.log2 / (10 : Nat).log2
    dbg_trace s!"h: {ed}, len: {len}"
    h * 10 * len + ed

def Calibration.check2 (c : Calibration) : Bool :=
  List.range (Nat.pow 3 (c.vs.length - 1)) |>.any (λx =>
    CalibrateWithOps2 x c.vs.reverse == c.test)

def part2 (input : String) : Nat :=
  parseInput input |>.filter Calibration.check2 |>.map (·.test) |>.foldl Nat.add 0

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day7s.txt"
  IO.print s!"{part1 input}\n"
  IO.print s!"{part2 input}\n"
  pure ()
