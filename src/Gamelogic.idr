module Gamelogic

import Data.Bits
import Effects
import Types

transposeMat : {n : Nat} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = replicate n []
transposeMat ([]::_) = []
transposeMat ((x :: xs) :: xss) = (x :: map head xss) :: transposeMat (xs :: map tail xss)
transposeMat (_ :: _) = ?transposeMat_missing_case_1

reverseMat : Vect n (Vect m a) -> Vect n (Vect m a)
reverseMat [] = []
reverseMat (x :: xs) = reverse x :: reverseMat xs

moveLeft : Vect n (Vect m Int) -> Vect n (Vect m Int)
moveLeft [] = []
moveLeft (x :: xs) = (myTighten $ myMerge $ myTighten x) :: moveLeft xs where
         myTighten : Vect n1 Int -> Vect n1 Int
         myTighten [] = []
         myTighten {n1 = S k}(x :: xs) =
                   if x==0 then
                      let result = myTighten xs ++ [0] in
                          rewrite plusCommutative 1 k in result
                   else x :: myTighten xs
         myMerge : Vect n1 Int -> Vect n1 Int
         myMerge [] = []
         myMerge y @ (x::[]) = y
         myMerge {n1 = S (S k)} (x :: y :: ys) = if x ==y then
                                                   [x+y, 0] ++ myMerge ys
                                                else x :: myMerge (y :: ys)

export
moves: {n, m: Nat} -> Direction -> Vect n (Vect m Int) -> Vect n (Vect m Int)
moves Left  = moveLeft
moves Right = reverseMat . moveLeft . reverseMat
moves Up    = transposeMat . moveLeft . transposeMat
moves Down  = transposeMat . moves Right . transposeMat

movePossible: {n, m: Nat} -> Vect n (Vect m Int) -> Bool
movePossible x = moves Left x /= x || moves Right x /= x ||
                 moves Up   x /= x || moves Down  x /= x

public export
data GameState = GameWin | GameOver | GameContinue

public export
gameWinOver : {n, m: Nat} -> Vect n (Vect m Int) -> Int -> GameState
gameWinOver x winvalue =
  if hasNum x winvalue then
    GameWin
  else
    if movePossible x || hasNum x 0
    then GameContinue
    else GameOver
    
where
  hasNum : Vect n1 (Vect m1 Int) -> Int -> Bool
  hasNum [] _ = False
  hasNum (x :: xs) number = rowhasValue x number || hasNum xs number
          where
            rowhasValue : Vect n2 Int -> Int -> Bool
            rowhasValue [] _ = False
            rowhasValue (x :: xs) value = x == value || rowhasValue xs value

countRowZero : Vect n Int -> Int
countRowZero [] = 0
countRowZero (x :: xs) = countRowZero xs + (if x==0 then 1 else 0)

countZero : Vect n (Vect m Int) -> Int
countZero [] = 0
countZero (x :: xs) = countRowZero x + countZero xs

export
addNum : Vect n (Vect m Int) -> Int -> Int -> Vect n (Vect m Int)
addNum [] _ _ = []
addNum (x :: xs) num pos = case countRowZero x < pos of
                                True => [x] ++ addNum xs num (pos - countRowZero x)
                                False => addRowNum x num pos :: xs
    where
      addRowNum : Vect n1 Int -> Int -> Int -> Vect n1 Int
      addRowNum [] _ _ = []
      addRowNum (x :: xs) num pos = case x == 0 of
                                         True => case pos > 1 of
                                                      True => [x] ++ addRowNum xs num (pos-1)
                                                      False => [num] ++ xs
                                         False => [x] ++ addRowNum xs num pos

export
arithInput : Int -> Int -> Int
arithInput x y = (\x => mod x y + 1) x

export
initMat : Has (Random Int) es => (n : Nat) -> (m : Nat) -> Eff es (Matrix n m Int)
initMat n m = do
  let numPos: Vect 4 Int = !(randomVect 4)
  pure $ initHelper n m (TwoFour $ arithInput (index 0 numPos) 10) 
    (arithInput (index 1 numPos) (natToInt m * natToInt n)) 
    (TwoFour $ arithInput (index 2 numPos) 10)
    (arithInput (index 3 numPos) (natToInt m * natToInt n)-1)
where
  initHelper : (n1 : Nat) -> (m1 : Nat) -> (num1 : Int) ->
                (pos1 : Int) -> (num2 : Int) -> (pos2 : Int) ->
                Vect n1 (Vect m1 Int)
  initHelper n1 m1 num1 pos1 num2 pos2 =
          addNum (addNum (replicate n1 $ replicate m1 0) num1 pos1) num2 pos2

  TwoFour : Int -> Int
  TwoFour x = if x > 8 then 4 else 2

  natToInt : Nat -> Int;
  natToInt 0 = 0;
  natToInt (S k) = 1+(natToInt k);

export
twoFour : Int -> Int
twoFour x = if (arithInput x 10) > 8 then 4 else 2

export
posAvail : Int -> Vect n (Vect m Int) -> Int
posAvail x mat = arithInput x (countZero mat)
