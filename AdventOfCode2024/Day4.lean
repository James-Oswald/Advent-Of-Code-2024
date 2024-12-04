
def sur := [(0,1), (1,0), (1,1), (1,-1), (0,-1), (-1,0), (-1,-1), (-1,1)]

def countXMASAux (m : Array (Array Char)) (i j di dj: Int) (l : List Char := "XMAS".data) : Nat :=
  match l with
  | [] => 1
  | h :: t =>
    if i >= 0 && j >= 0 && h == (m.getD i.toNat #[]).getD j.toNat '0' then
      countXMASAux m (i+di) (j+dj) di dj t
    else 0

def countXMAS (m : Array (Array Char)) : Nat :=
  (m.toList.enum.map (λ (i, x) =>
    (x.toList.enum.map (λ (j, _) =>
      (sur.map (λ (di, dj) =>
        countXMASAux m i j di dj
      )).foldl Nat.add 0
    )).foldl Nat.add 0
  )).foldl Nat.add 0

def stringToMatrix (s : String) : Array (Array Char) := ((s.splitOn "\n").map (λ x => x.data.toArray)).toArray

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

def getD (m : Array (Array Char)) (i j : Nat) : Char :=
  (m.getD i #[]).getD j '0'

def corners : Array (Int × Int) := #[(-1,-1), (-1,1), (1,-1), (1,1)]

def isX_MAS (m : Array (Array Char)) (i j : Nat) : Bool :=
  getD m i j == 'A' &&
  (corners.map (λ (di, dj) => countXMASAux m (i+di) (j+dj) (-di) (-dj) "MAS".data)).foldl Nat.add 0 == 2


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
  IO.print s!"{result}\n"
  let result := (countX_MAS ∘ stringToMatrix) input
  IO.print s!"{result}\n"
