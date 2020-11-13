{--module Eval where
import DataTypes
import Data.Map

toString :: Prim->String
toString (PBool x) = show x

toString :: Prim->String
toString (PInt x) = show x

toString :: Prim->String
toString (PDouble x) = show x

toString :: Prim->String
toString (PString x) = show x

toString :: Prim->String
toString (PChar x) = show x

data Type = Data String [Type] | Curry Type Type

data Value = Primitive Prim | Compound String [Value] | Func String [Value]

mas [Primitive (Prim PInt x), Primitive (Prim PInt y)] = Primitive (Prim $ x y)

toCurry :: [Type]->Type
toCurry (head: tail) = (Curry x $ toCurry tail)

mp = fromList [
 ("+", mas),
 ("-", toCurry [Primitive "Int", Primitive "Int"]),
--}
