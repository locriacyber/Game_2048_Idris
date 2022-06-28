module Effects

import public Control.Eff
import Types


-- ||| call/cc
-- public export
-- data Cont : Type -> Type where
--    CallNext :  -> Cont ()
-- 
-- callNext : Has Cont fs => m a -> Eff fs (m a)


||| @{a} random element type
public export
data Random : (a: Type) -> (Type -> Type) where
   RandomOne : Random a a

export
randomInt : Has (Random Int) es => Eff es Int
randomInt = send RandomOne

export
randomVect : Has (Random Int) es => (n: Nat) -> Eff es (Vect n Int)
randomVect Z = pure []
randomVect (S n) = [| randomInt :: randomVect n |]

||| Output. Display game stuff
public export
data DisplayInfo : Type -> Type where
   DisplayLine : String -> DisplayInfo ()
   DisplayMatrix : {m, n: Nat} -> Matrix m n Int -> DisplayInfo ()

export
info : Has (DisplayInfo) es => String -> Eff es ()
info = send . DisplayLine

export
displayState : 
   {m, n: Nat} ->
   Has (DisplayInfo) es => 
   Matrix m n Int ->
   Eff es ()
displayState = send . DisplayMatrix


public export
data InputEvent = Movement Direction | Quit | Invalid

||| Input
public export
data PlayerInput : Type -> Type where
   RequestOne : PlayerInput InputEvent

export
getInput : Has PlayerInput es => Eff es InputEvent
getInput = send RequestOne
