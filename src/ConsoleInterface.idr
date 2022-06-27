module ConsoleInterface

import System
import GameLoop
import Effects
import Types
import IORng

handleInput : PlayerInput a -> IO a
handleInput RequestOne = do
   act <- getLine
   pure $ case act of
      "w" => Movement Up
      "s" => Movement Down
      "a" => Movement Left
      "d" => Movement Right
      "q" => Quit
      _   => Invalid

displayMatrix : Matrix _ _ Int -> IO ()
displayMatrix [] = putStrLn " "
displayMatrix (x :: xs) = do
   printRow x
   putStrLn ""
   displayMatrix xs
where
   printRow : Vect n1 Int -> IO ()
   printRow [] = putStrLn ""
   printRow (x :: xs) = do
      putStr (show x ++ "  ")
      printRow xs


handleDisplay : DisplayInfo a -> IO a
handleDisplay (DisplayMatrix matrix) = displayMatrix matrix
handleDisplay (DisplayLine line) = putStrLn line

partial
main : IO ()
main = do
   let seed = cast !time
   runEff [handleInput, handleDisplay] (handleRandom seed $ main_pure forever)
