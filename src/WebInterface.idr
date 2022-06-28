module WebInterface

import System
import GameLoop
import Effects
import Types
import IORng
import WebContext
import JS
import Web.Dom
import Web.Html

-- %foreign "javascript:lambda:(s)=>window.prompt(s)"
-- export
-- prim__prompt : String -> PrimIO String

setCell : Int -> Element -> JSIO ()
setCell n el = do
   textContent el .= show n
   className el .= ("n" ++ show n)

export
displayMatrix : {m, n: Nat} -> JSContext -> Matrix m n Int -> JSIO ()
displayMatrix {m=4} {n=4} ctx mat = do
   ignore $ callEachMatrix setCell mat ctx.el_matrix 
   pure ()
displayMatrix _ _ = fatal "when_matrix_not_4x4"


handleDisplay : JSContext -> DisplayInfo a -> JSIO a
handleDisplay ctx (DisplayMatrix matrix) = displayMatrix ctx matrix
handleDisplay ctx (DisplayLine line) = do
   el_log_line <- createElement Div
   textContent el_log_line .= line
   ignore $ ctx.el_log `appendChild` el_log_line
   pure ()


-- prim_ : FunctionStringCallback -> JSIO 

-- (String -> JSIO ())

-- nextKeyEvent : KeyboardEvent -> JSIO ()
-- let handler : KeyboardEventHandler = callback nextKeyEvent

prompt : JSContext -> String -> JSIO (Maybe String)
prompt ctx message = do
   Window.prompt ctx.window (Def message) Undef


handleInput : JSContext -> PlayerInput a -> JSIO a
handleInput ctx RequestOne = do
   Just act <- prompt ctx "Choose: wasd q"
   | Nothing => pure Invalid
   pure $ case act of
      "w" => Movement Up
      "s" => Movement Down
      "a" => Movement Left
      "d" => Movement Right
      "q" => Quit
      _   => Invalid

partial
mainJS : JSIO ()
mainJS = do
   let seed = cast !time
   ctx <- try_make_ctx
   let main_cont0 = handleRandom seed $ main_pure forever
   runEff [handleInput ctx, handleDisplay ctx] main_cont0


partial
main : IO ()
main = do
   runJS $ mainJS
