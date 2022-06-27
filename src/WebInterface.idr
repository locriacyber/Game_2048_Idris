module WebInterface

import System
import GameLoop
import Effects
import Types
import IORng

%foreign "browser:lambda:(s)=>window.prompt(s)"
export
prim__prompt : String -> PrimIO String


handleInput : PlayerInput a -> IO a
handleInput RequestOne = do
   act <- fromPrim $ prim__prompt "Choose: wasd q"
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

export
partial
main : IO ()
main = do
   let seed = cast !time
   rng <- createRng seed
   runEff [handleInput, handleDisplay, rng] (main_pure forever)
