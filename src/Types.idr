||| pure types

module Types

import public Data.Vect
import public Data.Fuel

public export
Matrix : Nat -> Nat -> Type -> Type
Matrix m n a = Vect m (Vect n a)

callEachVect : Monad m => (a -> b -> m c) -> Vect x a -> Vect x b -> m (Vect x c)
callEachVect _ Nil Nil = pure Nil
callEachVect map_ (x :: xs) (y :: ys) =
   [| map_ x y :: callEachVect map_ xs ys |]

export
callEachMatrix : Monad m => (a -> b -> m c) -> Matrix x y a -> Matrix x y b -> m (Matrix x y c)
callEachMatrix _ Nil Nil = pure Nil
callEachMatrix map_ (x :: xs) (y :: ys) =
   [| callEachVect map_ x y :: callEachMatrix map_ xs ys |]


||| movement direction
public export
data Direction = Up | Down | Left | Right
