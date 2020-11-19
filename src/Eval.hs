module Eval where
import DataTypes
import Data.Map

data Data = DataInt Int | DataDouble Double | DataChar Char | DataBool Bool | DataConstr String [Data] deriving(Show)
data MatchResult = NoMatch | Match (Map String Data) deriving(Show)

instance Eq MatchResult where
	(==) NoMatch NoMatch = True
	(==) (Match e1) (Match e2) = True
	(==) _ _ = False

matchUnion :: [MatchResult] -> Map String Data
matchUnion [] = fromList $ []
matchUnion ((Match x): xl) = union x (matchUnion xl)

patternMatch :: Data->Pattern->MatchResult
patternMatch info (PatRef name) = Match $ fromList [(name, info)]
patternMatch (DataInt x) (PatLit (PInt y)) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataDouble x) (PatLit (PDouble y)) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataChar x) (PatLit (PChar y)) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataBool x) (PatLit (PBool y)) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataConstr "List" (x:xl)) (PatConstr "Cons" [y, z]) = 
	if ((patternMatch x y) == NoMatch || (patternMatch (DataConstr "List" xl) z) == NoMatch) then NoMatch
		else (Match (matchUnion [(patternMatch x y), (patternMatch (DataConstr "List" xl) z)]))
patternMatch (DataConstr x xl) (PatConstr y yl) = 
	if (x == y && (length xl) == (length yl) && (notElem NoMatch (zipWith patternMatch xl yl)))
		then Match (matchUnion (zipWith patternMatch xl yl)) else NoMatch
patternMatch _ _ = NoMatch



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
