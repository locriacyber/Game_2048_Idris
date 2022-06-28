module WebContext

import Types
import JS
import Web.Dom
import Web.Html

export
partial
fatal : String -> JSIO a
fatal message = do
   throwE $ Caught ("Error: " ++ message)

public export
record JSContext where
   constructor MkJSContext
   window : Window
   el_matrix : Matrix 4 4 Element
   el_log : Element
   poll_key : Lazy (JSIO KeyboardEvent)

--------
-- helpers for try_make_ctx

cast_helper : (HasIO io, ArrayLike arr a) => arr -> Nat -> io (List a)
cast_helper arr n = do
   Just x <- readIO arr (cast n)
   | Nothing => pure []
   pure $ x :: !(cast_helper arr (S n))

HasIO io => ArrayLike arr a => Cast arr (io (List a)) where
   cast arr = cast_helper arr 0

castListToElement : List Node -> Maybe (List Element)
castListToElement Nil = Just Nil
castListToElement (x :: xs) = [| safeCast x :: castListToElement xs |]

split : {m, n: Nat} -> Vect (S m * n) a -> (Vect n a, Vect (m * n) a)
split {n} xs = ((take n xs), (drop n xs))

stack : {m, n: Nat} -> Vect (m * n) a -> Matrix m n a
stack {m=Z} {n} _ = []
stack {m=S m'} {n} v = do
   let (first_layer, rest) = split {m=m'} {n} v
   first_layer :: stack {m=m'} {n} rest

partial
assert_length : (n: Nat) -> List a -> JSIO (Vect n a)
assert_length n el_list = do
   let Just el_vect = toVect n el_list
   | _ => fatal $ "Need 16, only " ++ (show (List.length el_list))
   pure el_vect

--------

export
try_make_ctx : JSIO JSContext
try_make_ctx = do
   window <- Web.Dom.window
   document <- Web.Dom.document
   Just el_log <- querySelector document ".log"
   | _ => fatal "no_el_content"
   node_list_cells <- querySelectorAll document ".cell .content"
   -- : List Node
   node_list <- cast {to=JSIO (List Node)} node_list_cells
   let Just el_list = castListToElement node_list
   | _ => fatal "node_not_element"
   el_vect <- assert_length 16 el_list
   let el_matrix = stack {m=4} {n=4} el_vect
   let poll_key = ?poll0
   pure $ MkJSContext {window} {el_matrix} {el_log} {poll_key}
