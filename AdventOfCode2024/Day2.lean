

def isSafe (l : List Int) : Bool :=
  let diffs := (l.zip l.tail).map (λ(v1, v2) => Int.natAbs (v1 - v2))
  (l.Pairwise (· < ·) || l.Pairwise (· > ·)) && diffs.all (λx => 1 ≤ x && x ≤ 3)

def isSafeWithRemove (l : List Int) : Bool :=
  let sublists := (List.range l.length).map (λn => l.eraseIdx n)
  sublists.any isSafe

def main : IO Unit := do
  let input ← IO.FS.lines "AdventOfCode2024/Day2.txt"
  let split := input.map (·.split (λc => c == ' '))
  let reports := Array.toList (split.map (λx => x.map (·.toInt!)))
  let safe := reports.filter isSafe
  IO.print s!"{safe.length}\n"
  let safeWithRemove := reports.filter isSafeWithRemove
  IO.print s!"{safeWithRemove.length}\n"
