module Main

import System
import Gamelogic
import Data.Fuel
import Data.Vect

game2048 : Fuel -> {n,m : Nat } -> Stream Int -> Matrix n m Int -> IO ()
game2048 Dry _ _ = pure ()
game2048 (More fuel) (num :: pos :: rands) x =
         do case gameWinOver x 2048 of
              GameWin => do putStrLn "Congratulations! You win the game!"
              GameOver => do putStrLn "Sorry! You lose the game!"
              GameContinue => do
                  putStrLn "The game continues!"
                  act <- getLine
                  case act of
                        "w" => if moves Up x == x then game2048 fuel rands x else
                              do newmat <- pure (addNum (moves Up x) (twoFour num) (posAvail pos x))
                                 printMat newmat
                                 game2048 fuel rands newmat
                        "s" => if moves Down x == x then game2048 fuel rands x else
                              do newmat <- pure $ addNum (moves Down x) (twoFour num) (posAvail pos x)
                                 printMat newmat
                                 game2048 fuel rands newmat
                        "a" => if moves Left x == x then game2048 fuel rands x else
                              do newmat <- pure $ addNum (moves Left x) (twoFour num) (posAvail pos x)
                                 printMat newmat
                                 game2048 fuel rands newmat
                        "d" => if moves Right x == x then game2048 fuel rands x else
                              do newmat <- pure $ addNum (moves Right x) (twoFour num) (posAvail pos x)
                                 printMat newmat
                                 game2048 fuel rands newmat
                        _   => do
                           putStrLn "The right operation is 'wasd'! "
                           game2048 fuel rands x

partial
main : IO ()
main = do seed <- time
          putStrLn "Welcome to 2048 game!"
          startMat <- pure (initMat (fromInteger seed) 4 4)
          printMat startMat
          usleep 1000000
          seed1 <- time
          game2048 forever (randoms (fromInteger seed1)) startMat
          putStrLn "Game end"
