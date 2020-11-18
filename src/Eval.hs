module Eval where
import DataTypes
import Data.Map

data Data = DataInt Int | DataDouble Double | DataChar Char | DataBool Bool | DataConstr String [Data] deriving(Show)
data MatchResult = NoMatch | Match (Map String Data) deriving(Show)

patternMatch :: Data->Pattern->MatchResult
patternMatch info (PatRef name) = Match $ fromList [(name, info)]
patternMatch info (PatLit (PInt x)) = case info of
 (DataInt x) -> Match $ fromList []
 _ -> NoMatch


{--
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
