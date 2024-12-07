
-- First Pass =================================================================
def part1 : IO Unit := do
  let input ← IO.FS.lines "AdventOfCode2024/Day1.txt"
  let spaced := input.map λs => s.replace "   " " "
  let split := spaced.map (·.split (λc => c == ' '))
  let numbers := split.map λl => (l[0]!.toInt!,l[1]!.toInt!)
  let ⟨l1, l2⟩ := numbers.unzip
  let l1s := l1.insertionSort (· < ·)
  let l2s := l2.insertionSort (· < ·)
  let pairs := l1s.zip l2s
  let diffs := pairs.map (λ(p1,p2) => Int.natAbs (p2 - p1))
  let sum := diffs.foldl (· + ·) 0
  IO.print s!"{sum}\n"

def part2 : IO Unit := do
  let input ← IO.FS.lines "AdventOfCode2024/Day1.txt"
  let spaced := input.map λs => s.replace "   " " "
  let split := spaced.map (·.split (λc => c == ' '))
  let numbers := split.map λl => (l[0]!.toInt!,l[1]!.toInt!)
  let ⟨l1, l2⟩ := numbers.unzip
  -- Clearly we could speed this up by using a hashmap, but
  -- I'm in functional land right now
  let occs := l1.map λn => n * l2.toList.count n
  let sum := occs.foldl (· + ·) 0
  IO.print s!"{sum}\n"

def main : IO Unit := do
  part1
  part2
  pure ()
