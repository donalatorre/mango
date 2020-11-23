module Eval where
import DataTypes
import Data.Map
import Data.Set
import Typing
import Control.Monad
import Text.Show.Pretty (ppShow)
import Control.Monad.State.Lazy

data ExecState = ExecState {
  execStore :: Map Int (TPattern, [TValue]),
  global :: Map String (Either Int Data),
  local :: Map String Data,
  visited :: Set Int,
  exec_var_count :: Int
} deriving (Show)

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
matchUnion [] = Data.Map.fromList $ []
matchUnion ((Match x): xl) = Data.Map.union x (matchUnion xl)

patternMatch :: Data->TPattern->MatchResult
patternMatch info (TPatRef name _) = Match $ Data.Map.fromList [(name, info)]
patternMatch (DataInt x) (TPatLit (PInt y) _) = if x == y then Match $ Data.Map.fromList [] else NoMatch
patternMatch (DataDouble x) (TPatLit (PDouble y) _) = if x == y then Match $ Data.Map.fromList [] else NoMatch
patternMatch (DataChar x) (TPatLit (PChar y) _) = if x == y then Match $ Data.Map.fromList [] else NoMatch
patternMatch (DataBool x) (TPatLit (PBool y) _) = if x == y then Match $ Data.Map.fromList [] else NoMatch
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
 (Match a, Match b)->Match $ Data.Map.union a b
patternListMatch _ _ = NoMatch

handleMultiDef :: [Data]->Data
handleMultiDef [x] = x
handleMultiDef (DataCall (Func lsta) [] argcA: DataCall (Func lstb) [] argcB: rest) = if argcA == argcB then
 handleMultiDef (DataCall (Func $ lsta++lstb) [] argcA: rest) else error $ "Fatal error: Different number of args"
handleMultiDef x = error $ "x is " ++ (ppShow x)

resolveBind :: TBind->State ExecState ()
resolveBind (TBindVal pat vls) = do
 vc <- exec_var_count <$> get
 markPat vc pat
 modify (\s->s{ execStore = Data.Map.insert vc (pat, vls) (execStore s), exec_var_count = vc + 1 })
 return ()
 where
  markPat :: Int->TPattern->State ExecState ()
  markPat vc (TPatRef name _) = modify (\s->s{ global = Data.Map.insert name (Left vc) (global s) }) >> pure ()
  markPat vc (TPatList lst _) = mapM_ (markPat vc) lst
  markPat vc (TPatConstr _ lst _) = mapM_ (markPat vc) lst
  markPat _ _ = pure ()

resolveAction stt (TAssign pat vls) = pure $ execState (resolveBind (TBindVal pat vls)) stt

resolveAction stt (TRead name) = do
 ln <- getLine
 return $ stt { global = Data.Map.insert name (Right $ DataConstr "List" $ Prelude.map DataChar ln) (global stt) }

resolveAction stt (TPrint vls) = do
 let (rVls, newStt) = runState (mapM resolve vls) stt
 let toPrint = Prelude.foldl (++) "" (Prelude.map show rVls)
 putStrLn toPrint
 return stt

resolveProgram (TProgram bds acts) = foldM_ resolveAction rBds acts
 where
  rBds = execState (mapM resolveBind bds) $ ExecState mempty primitiveCtx mempty Data.Set.empty 0

resolveStr :: String->State ExecState Data
resolveStr str = do
 lcl <- local <$> get
 gbl <- global <$> get
 ret <- (case (Data.Map.lookup str lcl, Data.Map.lookup str gbl) of
  (Just x, _) -> pure x
  (_, Just (Right dt)) -> pure dt
  (_, Just (Left idx)) -> do
   vis <- visited <$> get
   if Data.Set.member idx vis then error "Found loop in execution." else pure ()
   modify (\s -> s { visited = Data.Set.insert idx vis });
   stor <- execStore <$> get
   let (Just (fndPat, fndVlus)) = Data.Map.lookup idx stor
   rData <- handleMultiDef <$> mapM resolve fndVlus
   let patRes = patternMatch rData fndPat
   let newStuff = getMp patRes
   modify (\s -> s { visited = vis, global = Data.Map.union newStuff (global s) })
   finalRes <- resolveStr str
   return finalRes 
  _ -> error $ "Fatal error: var '"++str++"' not found in EXECUTION. This should've been noticed in Typing.")
 return ret
 where
  getMp :: MatchResult->Map String (Either Int Data)
  getMp NoMatch = error "Value-binding pattern match failed"
  getMp (Match mp) = (Data.Map.map (\y -> Right y) mp)

resolve :: TValue->State ExecState Data
resolve (TValCall name args _) = do
 rName <- resolveStr name
 rArgs <- mapM resolve args
 let unEvaled = reduceCall rName rArgs
 ret <- (case unEvaled of
  (DataCall fn lst 0)-> evaluate fn lst
  x->pure x)
 return ret
 where
  reduceCall x [] = x
  reduceCall (DataCall fn lst num) (arg: rest) = if num == 0 then error "Fatal error: Applying arg to non-function. This should've been noticed in Typing." else reduceCall (DataCall fn (arg: lst) (num - 1)) rest

resolve (TValList lst _) = liftM (DataConstr "List") (mapM resolve lst)
resolve (TValLambda ptrns ret _ _) = pure $ DataCall (Func [(ptrns, ret)]) [] (length ptrns)

resolve (TValLit (PInt x) _) = pure $ DataInt x
resolve (TValLit (PBool x) _) = pure $ DataBool x
resolve (TValLit (PDouble x) _) = pure $ DataDouble x
resolve (TValLit (PChar x) _) = pure $ DataChar x

argNums = [
 ("+", 2),
 ("-", 2),
 ("*", 2),
 ("/", 2),
 ("if", 3),
 ("++", 2)]

primitiveCtx = Data.Map.fromList $ Prelude.map (\(name, num) -> (name, Right $ DataCall (PrimFunc name) [] num)) argNums

evaluate :: Func->[Data]->State ExecState Data
evaluate a lst = evaluate' a (reverse lst)
 where
  evaluate' :: Func->[Data]->State ExecState Data
  evaluate' (PrimFunc "+") [DataInt a, DataInt b] = pure $ DataInt $ a + b
  evaluate' (PrimFunc "-") [DataInt a, DataInt b] = pure $ DataInt $ a - b
  evaluate' (PrimFunc "*") [DataInt a, DataInt b] = pure $ DataInt $ a * b
  evaluate' (PrimFunc "/") [DataInt a, DataInt b] = pure $ DataInt $ a `div` b
  evaluate' (PrimFunc "if") [DataBool cond, onTrue, onFalse] = pure $ if cond then onTrue else onFalse
  evaluate' (PrimFunc "++") [DataConstr "List" lsta, DataConstr "List" lstb] = pure $ DataConstr "List" $ lsta ++ lstb
  evaluate' (PrimFunc _) _ = error "Fatal error: wrong call to primitive function"
  
  evaluate' (Func opts) args = do
   let revArgs = reverse args
   toRet <- tryEval revArgs opts
   return toRet
   where
    tryEval revArgs [] = error "Non-exhaustive function"
    tryEval revArgs ((ptrns, ret):xs) = case patternListMatch revArgs ptrns of
     NoMatch-> tryEval revArgs xs
     Match lcl -> do
      old <- local <$> get
      modify (\s->s { local = lcl })
      rData <- resolve ret
      modify (\s->s { local = old })
      return rData

