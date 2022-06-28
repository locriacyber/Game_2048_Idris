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
import Web.Raw.UIEvents

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

handleInput : JSContext -> Has JSIO fs => PlayerInput v -> (v -> Eff fs a) -> Eff fs a
handleInput ctx RequestOne resume = do
   keyevent <- send ctx.poll_key
   key_s <- send $ KeyboardEvent.key keyevent
   resume $ case key_s of
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
   let main_cont1 = lift1 {f=JSIO} main_cont0
   let main_cont2 = handle {f=PlayerInput} (handleInput ctx) main_cont1
   runEff [id, handleDisplay ctx] main_cont2


partial
main : IO ()
main = do
   runJS $ mainJS
