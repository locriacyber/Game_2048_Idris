module Main

import Data.Primitives.Views
import Data.Vect
import Data.Nat
import System

transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat ([]::_) = []
transposeMat ((x :: xs) :: xss) = (x :: map head xss) :: transposeMat (xs :: map tail xss)

reverseMat : Vect n (Vect m elem) -> Vect n (Vect m elem)
reverseMat [] = []
reverseMat (x :: xs) = reverse x :: reverseMat xs

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

moves: {m, n : _} -> Action -> Vect n (Vect m Int) -> Vect n (Vect m Int)
moves Left  = moveLeft
moves Right = reverseMat . moveLeft . reverseMat
moves Up    = transposeMat . moveLeft . transposeMat
moves Down  = transposeMat . moves Right . transposeMat

movePossible: {m, n : _} -> Vect n (Vect m Int) -> Bool
movePossible x = moves Left x /= x || moves Right x /= x ||
                 moves Up   x /= x || moves Down  x /= x


data GameState = GameWin | GameOver | GameContinue

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

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   abs (seed' `shiftR` 2) :: randoms seed'

arithInput : Int -> Int -> Int
arithInput x y = (\x => mod x y + 1) x
                        
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


initMatIO : Vect n (Vect m Int) -> IO (Vect n (Vect m Int))
initMatIO x = pure x

printMat : Vect n (Vect m Int) -> IO()
printMat [] = putStrLn " "
printMat (x :: xs) = do printRow x
                        putStrLn ""
                        printMat xs
    where
      printRow : Vect n1 Int -> IO ()
      printRow [] = putStrLn ""
      printRow (x :: xs) = do putStr (show x ++ "  ")
                              printRow xs

data RunIO : Type where
     Quit : RunIO
     Do : IO a -> (a -> Inf (RunIO )) -> RunIO

(>>=) : IO a -> (a -> Inf (RunIO)) -> RunIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO -> IO ()
run fuel Quit = pure ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure ()

partial
forever : Fuel
forever = More forever

game2048 : {n,m : _ } -> Stream Int -> (Vect n (Vect m Int)) -> RunIO
game2048 (num :: pos :: rands) x =
         do case gameWinOver x 2048 of
              GameWin => do putStrLn "Congratulations! You win the game!"
                            Quit
              GameOver => do putStrLn "Sorry! You lose the game!"
                             Quit
              GameContinue => do putStrLn "The game continues!"
                                 act <- getLine
                                 case act of
                                      "w" => if moves Up x == x then game2048 rands x else
                                             do newmat <- pure (addNum (moves Up x) (TwoFour num) (posAvail pos x))
                                                printMat newmat
                                                game2048 rands newmat
                                      "s" => if moves Down x == x then game2048 rands x else
                                             do newmat <- pure $ addNum (moves Down x) (TwoFour num) (posAvail pos x)
                                                printMat newmat
                                                game2048 rands newmat
                                      "a" => if moves Left x == x then game2048 rands x else
                                             do newmat <- pure $ addNum (moves Left x) (TwoFour num) (posAvail pos x)
                                                printMat newmat
                                                game2048 rands newmat
                                      "d" => if moves Right x == x then game2048 rands x else
                                             do newmat <- pure $ addNum (moves Right x) (TwoFour num) (posAvail pos x)
                                                printMat newmat
                                                game2048 rands newmat
                                      _   => do putStrLn "The right operation is 'wasd'! "
                                                game2048 rands x
    where
      TwoFour : Int -> Int
      TwoFour x = if (arithInput x 10) > 8 then 4 else 2

      posAvail : Int -> Vect n (Vect m Int) -> Int
      posAvail x mat = arithInput x (countZero mat)

partial
main : IO()
main = do seed <- time
          putStrLn "Welcome to 2048 game!"
          startMat <- pure (initMat (fromInteger seed) 4 4)
          printMat startMat
          usleep 1000000
          seed1 <- time
          run forever (game2048 (randoms (fromInteger seed1)) startMat)
          putStrLn "Game end"