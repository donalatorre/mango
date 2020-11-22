module Eval where
import DataTypes
import Data.Map
import Typing
import Control.Monad
import Text.Show.Pretty (ppShow)

data Func = PrimFunc String -- primitive function's name
 | Func [([TPattern], TValue)] deriving(Show)

data Data = DataInt Int
 | DataDouble Double
 | DataChar Char
 | DataBool Bool
 | DataConstr String [Data]
 | DataCall Func [Data] Int -- List represents args called so far. Int represents how many args are still needed.
 deriving(Show)

data MatchResult = NoMatch | Match (Map String Data) deriving(Show)

instance Eq MatchResult where
  (==) NoMatch NoMatch = True
  (==) (Match e1) (Match e2) = True
  (==) _ _ = False

matchUnion :: [MatchResult] -> Map String Data
matchUnion [] = fromList $ []
matchUnion ((Match x): xl) = union x (matchUnion xl)

patternMatch :: Data->TPattern->MatchResult
patternMatch info (TPatRef name _) = Match $ fromList [(name, info)]
patternMatch (DataInt x) (TPatLit (PInt y) _) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataDouble x) (TPatLit (PDouble y) _) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataChar x) (TPatLit (PChar y) _) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataBool x) (TPatLit (PBool y) _) = if x == y then Match $ fromList [] else NoMatch
patternMatch (DataConstr "List" (x:xl)) (TPatConstr "Cons" [y, z] _) = 
  if ((patternMatch x y) == NoMatch || (patternMatch (DataConstr "List" xl) z) == NoMatch) then NoMatch
    else (Match (matchUnion [(patternMatch x y), (patternMatch (DataConstr "List" xl) z)]))
patternMatch (DataConstr x xl) (TPatConstr y yl _) = 
  if (x == y && (length xl) == (length yl) && (notElem NoMatch (zipWith patternMatch xl yl)))
    then Match (matchUnion (zipWith patternMatch xl yl)) else NoMatch
patternMatch _ _ = NoMatch

patternListMatch :: [Data]->[TPattern]->MatchResult
patternListMatch [] [] = Match mempty
patternListMatch (dt:dts) (ptrn:ptrns) = case (patternMatch dt ptrn, patternListMatch dts ptrns) of
 (NoMatch, _)->NoMatch
 (_, NoMatch)->NoMatch
 (Match a, Match b)->Match $ union a b
patternListMatch _ _ = NoMatch

handleMultiDef :: [Data]->Data
handleMultiDef [x] = x
handleMultiDef (DataCall (Func lsta) [] argcA: DataCall (Func lstb) [] argcB: rest) = if argcA == argcB then
 handleMultiDef (DataCall (Func $ lsta++lstb) [] argcA: rest) else error $ "Fatal error: Different number of args"
handleMultiDef x = error $ "x is " ++ (ppShow x)

resolveBind :: Map String Data->TBind->Map String Data
resolveBind ctx (TBindVal pat vls) = case matched of
 NoMatch -> error "Non-exhaustive patterns in let"
 Match mp -> union mp ctx
 where
  matched = patternMatch dt pat
  dt = handleMultiDef $ Prelude.map (resolve ctx) vls

resolveAction ctx (TAssign pat vls) = pure $ case matched of
 NoMatch -> error "Non-exhaustive patterns in var"
 Match mp -> union mp ctx
 where
  matched = patternMatch dt pat
  dt = handleMultiDef $ Prelude.map (resolve ctx) vls

resolveAction ctx (TRead name) = do
 ln <- getLine
 return $ insert name (DataConstr "List" $ Prelude.map DataChar ln) ctx

resolveAction ctx (TPrint vls) = do
 let toPrint = Prelude.foldl (++) "" $ Prelude.map (show . (resolve ctx)) vls
 putStrLn toPrint
 return ctx

resolveStr :: Map String Data->String->Data
resolveStr ctx str = case Data.Map.lookup str ctx of
 Just x -> x
 _ -> error $ "Fatal error: var '"++str++"' not found in EXECUTION. This should've been noticed in Typing."

resolve :: Map String Data->TValue->Data
resolve ctx (TValCall name args _) = case unEvaled of
 (DataCall fn lst 0)-> evaluate ctx fn lst
 x->x
 where
  unEvaled = reduceCall (resolveStr ctx name) (Prelude.map (resolve ctx) args)
  reduceCall x [] = x
  reduceCall (DataCall fn lst num) (arg: rest) = if num == 0 then error "Fatal error: Applying arg to non-function. This should've been noticed in Typing." else reduceCall (DataCall fn (arg: lst) (num - 1)) rest

resolve ctx (TValList lst _) = DataConstr "List" (Prelude.map (resolve ctx) lst)
resolve ctx (TValLambda ptrns ret _ _) = DataCall (Func [(ptrns, ret)]) [] (length ptrns)

resolve ctx (TValLit (PInt x) _) = DataInt x
resolve ctx (TValLit (PBool x) _) = DataBool x
resolve ctx (TValLit (PDouble x) _) = DataDouble x
resolve ctx (TValLit (PChar x) _) = DataChar x

resolveProgram (TProgram bds acts) = foldM_ resolveAction rBds acts
 where
  rBds = Prelude.foldl resolveBind primitiveCtx bds

argNums = [
 ("+", 2),
 ("-", 2),
 ("*", 2),
 ("/", 2),
 ("if", 3),
 ("++", 2)]

primitiveCtx = fromList $ Prelude.map (\(name, num) -> (name, DataCall (PrimFunc name) [] num)) argNums

evaluate :: Map String Data->Func->[Data]->Data
evaluate a b lst = evaluate' a b (reverse lst)
 where
  evaluate' _ (PrimFunc "+") [DataInt a, DataInt b] = DataInt $ a + b
  evaluate' _ (PrimFunc "-") [DataInt a, DataInt b] = DataInt $ a - b
  evaluate' _ (PrimFunc "*") [DataInt a, DataInt b] = DataInt $ a * b
  evaluate' _ (PrimFunc "/") [DataInt a, DataInt b] = DataInt $ a `div` b
  evaluate' _ (PrimFunc "if") [DataBool cond, onTrue, onFalse] = if cond then onTrue else onFalse
  evaluate' _ (PrimFunc "++") [DataConstr "List" lsta, DataConstr "List" lstb] = DataConstr "List" $ lsta ++ lstb
  evaluate' _ (PrimFunc _) _ = error "Fatal error: wrong call to primitive function"
  
  evaluate' mp (Func opts) args = tryEval opts
   where
    revArgs = reverse args
    tryEval [] = error "Non-exhaustive function"
    tryEval ((ptrns, ret):xs) = case patternListMatch revArgs ptrns of
     NoMatch-> tryEval xs
     Match lcl -> resolve (union lcl mp) ret

