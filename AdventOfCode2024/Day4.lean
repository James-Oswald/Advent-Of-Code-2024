
def sur := #[(0,1), (1,0), (1,1), (1,-1), (0,-1), (-1,0), (-1,-1), (-1,1)]
def corners : Array (Int × Int) := #[(-1,-1), (-1,1), (1,-1), (1,1)]

def stringToMatrix (s : String) : Array (Array Char) :=
  ((s.splitOn "\n").map (λ x => x.data.toArray)).toArray

def getD (m : Array (Array Char)) (i j : Int) : Char :=
  if i >= 0 && j >= 0 then (m.getD i.toNat #[]).getD j.toNat '0' else '0'

-- Part 1

def countXMASAux (m : Array (Array Char)) (i j di dj: Int) (l : List Char := "XMAS".data) : Nat :=
  match l with
  | [] => 1
  | h :: t => if h == getD m i j then countXMASAux m (i+di) (j+dj) di dj t else 0

def countXMAS (m : Array (Array Char)) : Nat := Id.run do
  (m.toList.enum.map (λ (i, x) =>
    (x.toList.enum.map (λ (j, _) =>
      (sur.map (λ (di, dj) =>
        countXMASAux m i j di dj
      )).foldl Nat.add 0
    )).foldl Nat.add 0
  )).foldl Nat.add 0

#eval (countXMAS ∘ stringToMatrix) "XMAS"

#eval (countXMAS ∘ stringToMatrix) "SAMX"

#eval (countXMAS ∘ stringToMatrix) "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"

-- Part 2

def isX_MAS (m : Array (Array Char)) (i j : Nat) : Bool :=
  getD m i j == 'A' && (corners.map (λ (di, dj) =>
    countXMASAux m (i+di) (j+dj) (-di) (-dj) "MAS".data
  )).foldl Nat.add 0 == 2

def countX_MAS (m : Array (Array Char)) : Nat :=
  (m.toList.enum.map (λ (i, x) =>
    (x.toList.enum.map (λ (j, _) =>
      if isX_MAS m i j then 1 else 0
    )).foldl Nat.add 0
  )).foldl Nat.add 0

#eval (countX_MAS ∘ stringToMatrix)
".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."

def main : IO Unit := do
  let input ← IO.FS.readFile "AdventOfCode2024/Day4.txt"
  let result := (countXMAS ∘ stringToMatrix) input
  IO.print s!"Part 1 {result}\n"
  let result := (countX_MAS ∘ stringToMatrix) input
  IO.print s!"Part 2 {result}\n"
