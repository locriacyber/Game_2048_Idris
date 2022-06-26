module Gamelogic
import Data.Vect

import Data.Nat
import Data.Bits

public export
Matrix : Nat -> Nat -> Type -> Type
Matrix m n a = Vect m (Vect n a)

transposeMat : {n : _} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = replicate _ []
transposeMat ([]::_) = []
transposeMat ((x :: xs) :: xss) = (x :: map head xss) :: transposeMat (xs :: map tail xss)
transposeMat (_ :: _) = ?transposeMat_missing_case_1

reverseMat : Vect n (Vect m a) -> Vect n (Vect m a)
reverseMat [] = []
reverseMat (x :: xs) = reverse x :: reverseMat xs

public export
data Action = Up | Down | Left | Right | Restart | Exit

moveLeft : {m : _} -> Vect n (Vect m Int) -> Vect n (Vect m Int)
moveLeft [] = []
moveLeft (x :: xs) = (myTighten $ myMerge $ myTighten x) :: moveLeft xs where
         myTighten : {n1 : _} -> Vect n1 Int -> Vect n1 Int
         myTighten [] = []
         myTighten {n1 = S k}(x :: xs) =
                   if x==0 then
                      let result = myTighten xs ++ [0] in
                          rewrite plusCommutative 1 k in result
                   else x :: myTighten xs
         myMerge : {n1 : _} -> Vect n1 Int -> Vect n1 Int
         myMerge [] = []
         myMerge y @ (x::[]) = y
         myMerge {n1 = S (S k)} (x :: y :: ys) = if x ==y then
                                                   [x+y, 0] ++ myMerge ys
                                                else x :: myMerge (y :: ys)

export
moves: {m, n : _} -> Action -> Vect n (Vect m Int) -> Vect n (Vect m Int)
moves Left  = moveLeft
moves Right = reverseMat . moveLeft . reverseMat
moves Up    = transposeMat . moveLeft . transposeMat
moves Down  = transposeMat . moves Right . transposeMat
moves Restart = ?moves_missing_case_1
moves Exit = ?moves_missing_case_2

movePossible: {m, n : _} -> Vect n (Vect m Int) -> Bool
movePossible x = moves Left x /= x || moves Right x /= x ||
                 moves Up   x /= x || moves Down  x /= x

public export
data GameState = GameWin | GameOver | GameContinue

public export
gameWinOver : {m, n : _} -> Vect n (Vect m Int) -> Int -> GameState
gameWinOver x winvalue = case hasNum x winvalue of
                              True => GameWin
                              False => case movePossible x || hasNum x 0 of
                                            False => GameOver
                                            True => GameContinue
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
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   abs (seed' `shiftR` 2) :: randoms seed'

export
arithInput : Int -> Int -> Int
arithInput x y = (\x => mod x y + 1) x
                        
export
initMat : (seed : Int) -> (n : Nat) -> (m : Nat) -> Vect n (Vect m Int)
initMat seed n m  = let numPos = fromList $ take 4 $ randoms seed in
                        initHelper n m (TwoFour $ arithInput (index 0 numPos) 10) 
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
printMat : Vect n (Vect m Int) -> IO ()
printMat [] = putStrLn " "
printMat (x :: xs) = do printRow x
                        putStrLn ""
                        printMat xs
    where
      printRow : Vect n1 Int -> IO ()
      printRow [] = putStrLn ""
      printRow (x :: xs) = do putStr (show x ++ "  ")
                              printRow xs

export
twoFour : Int -> Int
twoFour x = if (arithInput x 10) > 8 then 4 else 2

export
posAvail : Int -> Vect n (Vect m Int) -> Int
posAvail x mat = arithInput x (countZero mat)
