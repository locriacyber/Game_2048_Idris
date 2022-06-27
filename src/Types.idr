||| pure types

module Types

import public Data.Vect
import public Data.Fuel

public export
Matrix : Nat -> Nat -> Type -> Type
Matrix m n a = Vect m (Vect n a)

||| movement direction
public export
data Direction = Up | Down | Left | Right
