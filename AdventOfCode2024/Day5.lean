
import Std.Data.HashMap
import Std.Data.HashSet

-- Notation for big O
-- r : number of rules in the input
-- u : number of updates
-- s : length of (the longest) update

instance : ToString (Std.HashSet Nat) where
  toString s := (s.toList.map toString) |>.map (·++",") |> String.join

--O(rus)
def parseInput (input : String) : List (Nat × Nat) × (List (List Nat)) :=
  let secs := input.splitOn "\n\n"
  let rules := (secs[0]!.splitOn "\n")|>.map (λ x => let rs := x.splitOn "|"; (rs[0]!.toNat!, rs[1]!.toNat!))
  let updates := (secs[1]!.splitOn "\n") |>.map (·.splitOn ",") |>.map (λx => x.map (λs => s.toNat!))
  (rules, updates)

--O(r)
def beforeMap : List (Nat × Nat) → Std.HashMap Nat (Std.HashSet Nat)
| [] => Std.HashMap.empty
| (a, b) :: t =>
  let m := beforeMap t
  match m[b]? with
  | none => m.insert b (Std.HashSet.ofList [a])
  | some h => m.insert b (h.insert a)

--O(s^2)
def ruleLegal (bm : Std.HashMap Nat (Std.HashSet Nat)) : List Nat -> Option (Nat × Nat)
| [] => none
| h::t =>
  let bs := bm[h]?;
  if H1 : bs.isSome then
    let f := t.find? ((bs.get H1).contains ·)
    if H2 : f.isSome then
      some (h, f.get H2)
    else
      ruleLegal bm t
  else
    ruleLegal bm t

--O(s) since l is a linked list
def List.middle (l : List Nat) : Nat :=
  l.get! (l.length / 2)

--O(rsu^2)
def part1 (input : String) : Nat :=
  let (rules, updates) := parseInput input
  let bm := beforeMap rules
  let legal := updates.filter (ruleLegal bm · |>.isNone)
  legal |>.map (List.middle) |>.foldl Nat.add 0

--O(ru^2)
def fixIllegal (bm : Std.HashMap Nat (Std.HashSet Nat)) (illegal : List Nat) : List Nat := Id.run do
  let mut ill := illegal.toArray;
  while ruleLegal bm ill.toList |>.isSome do       --O(u^2) at most r times 
    let (a, b) := ruleLegal bm ill.toList |>.get!
    let i := ill.toList.indexOf a
    let j := ill.toList.indexOf b
    ill := ill.swap! i j
  ill.toList

--(rsu^2)
def part2 (input : String) : Nat :=
  let (rules, updates) := parseInput input
  let bm := beforeMap rules
  updates.filter (ruleLegal bm · |>.isSome) |>.map (fixIllegal bm ·) |>.map (List.middle) |>.foldl Nat.add 0

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day5.txt"
  IO.print s!"{part1 input}\n"
  IO.print s!"{part2 input}\n"
