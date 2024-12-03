
-- The world's ugliest parser
-- No im not figuring out how to use Regex, I'm not a Normie.
partial def part1 (s : String) : Nat :=
  match s.toList with
  | [] => 0
  | 'm' :: 'u' :: 'l' :: '(' :: t =>
    let n1 := (t.takeWhile (·.isDigit)).asString.toNat!
    match t.dropWhile (·.isDigit) with
    | [] => 0
    | ',' :: t =>
      let n2 := (t.takeWhile (·.isDigit)).asString.toNat!
      match t.dropWhile (·.isDigit) with
      | [] => 0
      | ')' :: t => n1 * n2 + part1 t.asString
      | _ :: t => part1 t.asString
    | _ :: t => part1 t.asString
  | _ :: t => part1 t.asString

#eval part1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

partial def part2 (s : String) (flag : Bool := true) : Nat :=
  match s.toList with
  | [] => 0
  | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: t => part2 t.asString false
  | 'd' :: 'o' :: '(' :: ')' :: t => part2 t.asString true
  | 'm' :: 'u' :: 'l' :: '(' :: t =>
    let n1 := (t.takeWhile (·.isDigit)).asString.toNat!
    match t.dropWhile (·.isDigit) with
    | [] => 0
    | ',' :: t =>
      let n2 := (t.takeWhile (·.isDigit)).asString.toNat!
      match t.dropWhile (·.isDigit) with
      | [] => 0
      | ')' :: t => ite flag (n1 * n2) 0 + part2 t.asString flag
      | _ :: t => part2 t.asString flag
    | _ :: t => part2 t.asString flag
  | _ :: t => part2 t.asString flag

#eval part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day3.txt"
  IO.print s!"{part1 input}\n"
  IO.print s!"{part2 input}\n"
