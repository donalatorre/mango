{--
  Typing.hs is the module that receives the AST generated by Lexer.hs
  It handles the type inference using the DSU algorithm
  Main Module uses this module and passes the information to the evaluator
--}
{-# OPTIONS_GHC -XBangPatterns #-}
module Typing where

import DataTypes
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Map
import Data.Set
import Control.Monad
import Text.Show.Pretty (ppShow)

data TBind = TBindVal TPattern [TValue] deriving (Show) -- TODO: enable type constraints
data TPattern = TPatLit Prim Type | TPatConstr String [TPattern] Type | TPatList [TPattern] Type | TPatRef String Type deriving (Show)
data TValue = TValLit Prim Type
 | TValConstr String [TValue] Type
 | TValCall String [TValue] Type
 | TValList [TValue] Type
 | TValLambda [TPattern] TValue [TBind] Type deriving (Show)
data TAction = TAssign TPattern [TValue] | TPrint [TValue] | TRead String | TReadM Int String deriving (Show)
data TProgram = TProgram [TBind] [TAction] deriving (Show)

data Type = TConstr String [Type] | TVar Int (Set String) | TInst Type Type deriving (Show)


class Typed a where
 getType :: a -> State InferState Type

instance Typed TPattern where
 getType (TPatLit _ tp) = find tp
 getType (TPatConstr _ _ tp) = find tp
 getType (TPatList _ tp) = find tp
 getType (TPatRef _ tp) = find tp
instance Typed TValue where
 getType (TValLit _ tp) = find tp
 getType (TValConstr _ _ tp) = find tp
 getType (TValCall _ _ (TInst _ tp)) = find tp
 getType (TValCall _ _ otr) = find otr
 getType (TValList _ tp) = find tp
 getType (TValLambda _ _ _ tp) = find tp

data InferState = InferState {
  insts :: Set (String, String),
  store :: Map Int Type,
  gContext :: Map String Type,
  lContext :: Map String Type,
  var_count :: Int,
  --TODO: change into multiple state
  specMap :: Map Int Type
} deriving (Show)

runConstr (TConstr a args) (TConstr b brgs) fun = if a /= b then error ("Couldn't match '"++a++"'"++" with '"++b++"'") else checkedResult
 where
  checkedResult = if (length args) == (length brgs) then result else error "Constructors don't have the same number of arguments"
  result = do
   lst <- fun args brgs
   return $ TConstr a lst

unify :: Type->Type->State InferState Type
unify a b = do
 prntA <- find a
 prntB <- find b
 unified <- unify' prntA prntB
 return unified
 where
  unify' :: Type->Type->State InferState Type
  unify' x@(TConstr _ _) y@(TConstr _ _) = runConstr x y (zipWithM unify')
  unify' a@(TConstr _ _) b@(TVar _ _) = unify' b a
  unify' (TVar x xc) b = do
   newTp <- satisfy b (Data.Set.toList xc)
   modify (\s->s { store = Data.Map.insert x newTp (store s) })
   return newTp

satisfy :: Type->[String]->State InferState Type
satisfy tp cns = do
 inst <- insts <$> get
 ret <- satisfy' tp cns inst
 return ret
 where
  satisfy' :: Type->[String]->Set (String, String)->State InferState Type
  satisfy' x@(TVar _ _) cnst _ = do
   fX <- find x
   let (TVar y yc) = fX
   let newTp = TVar y $ Data.Set.union yc $ Data.Set.fromList cnst
   modify (\s->s{store = Data.Map.insert y newTp (store s)})
   return newTp
  satisfy' b@(TConstr _ _) [] insts = pure b
  satisfy' b@(TConstr nm []) (x: xs) insts = if Data.Set.member (x, nm) insts then satisfy' b xs insts else error $ "No instance '"++x++"' for type '"++nm++"'."
  -- For constructors with at least one arg, error out
  satisfy' (TConstr _ (_:_)) _ _ = error "Can't instantiate classes for high order types."

find :: Type->State InferState Type

find (TConstr nm args) = do
 evld <- mapM find args
 return $ TConstr nm evld

find (TVar x _) = do
 str <- store <$> get
 let y = fromJust $ Data.Map.lookup x str
 newY <- case y of 
  TVar ff _ -> if ff == x then pure y else find y
  _ -> find y
 modify (\s -> s { store = Data.Map.insert x newY (store s) })
 return newY

specify :: Type->Type->State InferState Type
specify a b = do
 prntA <- find a
 prntB <- find b
 res <- specify' prntA prntB
 return res
 where
  specify' :: Type->Type->State InferState Type
  specify' x@(TConstr _ _) y@(TConstr _ _) = runConstr x y (zipWithM specify')
  specify' con@(TConstr a args) (TVar x cns) = do
   -- This call should have no effect, just some checks.
   satisfy con $ Data.Set.toList cns
   brgs <- replicateM (length args) newMetaVar
   res <- zipWithM specify' args brgs
   let newTp = (TConstr a res)
   modify (\s -> s { store = Data.Map.insert x newTp (store s) })
   return newTp
  specify' (TVar x cns) b = do
   spec <- specMap <$> get
   let matched = Data.Map.lookup x spec
   ret <- if isJust matched then unify b (fromJust matched) else do
    constrainedB <- satisfy b (Data.Set.toList cns)
    modify (\s -> s {specMap = Data.Map.insert x constrainedB (specMap s)})
    return constrainedB
   return ret

newMetaVar :: State InferState Type
newMetaVar = do
 v <- var_count <$> get
 modify (\s -> s {store = Data.Map.insert v (TVar v Data.Set.empty) (store s), var_count = v + 1})
 return $ TVar v Data.Set.empty

destructFun :: Type->[Type]->Type
destructFun typ [] = typ
destructFun (TConstr "TFun" [_, body]) (_: rest) = destructFun body rest

typeCall :: [Type]->Type
typeCall [] = error "Empty list"
typeCall [x] = x
typeCall (x: xs) = TConstr "TFun" [x, typeCall xs]

typePrim prm = case prm of
 PString _ -> TConstr "List" [TConstr "Char" []]
 _ -> TConstr (case prm of
  PBool _ -> "Bool"
  PInt _ -> "Int"
  PDouble _ -> "Double"
  PChar _ -> "Char") []

typeVal :: Value->State InferState TValue
typeVal (ValLit prm) = pure $ TValLit prm $ typePrim prm
typeVal (ValCall name args) = do
 ret <- newMetaVar
 typedArgs <- mapM typeVal args
 lCtx <- lContext <$> get
 gCtx <- gContext <$> get
 finalTyp <- case (Data.Map.lookup name lCtx, Data.Map.lookup name gCtx) of
  (Nothing, Nothing) -> error ("Variable '"++name++"' not in scope.")
  (Just lclVr, Nothing) -> getFuncType typedArgs ret lclVr
  (Nothing, Just gblVr) -> pure $ TInst gblVr ret
 topr <- store <$> get
 otrpr <- lContext <$> get
 return $ TValCall name typedArgs finalTyp
 where
  getFuncType typedArgs ret vr = do
   onlyTypes <- mapM getType typedArgs
   let funcd = typeCall (onlyTypes++[ret])
   unified <- unify vr funcd
   return $ destructFun unified onlyTypes

typeVal (ValLambda (args, ret) _) = do
 oldCtx <- lContext <$> get
 typedArgs <- mapM (typePattern True) args
 onlyTypes <- mapM getType typedArgs
 typedRet <- typeVal ret
 onlyRetType <- getType typedRet
 let funcTyp = arrow onlyTypes onlyRetType
 -- Go back to original local lContext
 modify (\s -> s {lContext = oldCtx})
 return $ TValLambda typedArgs typedRet [] funcTyp
 where
  arrow :: [Type]->Type->Type
  arrow [] ret = ret
  arrow (hd: tl) ret = TConstr "TFun" [hd, arrow tl ret]

typeVal (ValList args) = do
 vr <- newMetaVar
 typedArgs <- mapM typeVal args
 onlyTypes <- mapM getType typedArgs
 finalVr <- foldM unify vr onlyTypes
 return $ TValList typedArgs $ TConstr "List" [finalVr]

typeVal (ValConstr name args) = do
 tArgs <- mapM typeVal args
 onlyTypes <- mapM getType tArgs
 ret <- if name == "Cons" then tpCons onlyTypes else error "Constructors other than Cons not implemented yet"
 return $ TValConstr name tArgs ret

tpCons :: [Type]->State InferState Type
tpCons [headType, restType] = do
 let lstTp = TConstr "List" [headType]
 ret <- unify restType lstTp
 return ret
tpCons (a: b: c) = do
 ntp <- unify a b
 ret <- tpCons (ntp: c)
 return ret
tpCons _ = error "Wrong use of Cons"

typePattern :: Bool->Pattern->State InferState TPattern
typePattern isArg (PatRef name) = do
 lCtx <- lContext <$> get
 gCtx <- gContext <$> get
 let ctx = if isArg then lCtx else gCtx
 if Data.Map.member name lCtx || Data.Map.member name gCtx then error ("Variable '"++name++"' already exists.") else do
   vr <- newMetaVar
   if name == "_" then pure () else modify (\s -> if isArg then (s { lContext = Data.Map.insert name vr ctx}) else (s {gContext = Data.Map.insert name vr ctx}))
   return $ TPatRef name vr
typePattern _ (PatLit prm) = pure $ TPatLit prm $ typePrim prm
typePattern isArg (PatConstr name args) = do
 tArgs <- mapM (typePattern isArg) args
 onlyTypes <- mapM getType tArgs
 ret <- if name == "Cons" then tpCons onlyTypes else error "Constructors other than Cons not implemented yet"
 return $ TPatConstr name tArgs ret

typePattern isArg (PatList lst) = do
 tArgs <- mapM (typePattern isArg) lst
 onlyTypes <- mapM getType tArgs
 vr <- newMetaVar
 ntp <- foldM unify vr onlyTypes
 return $ TPatList tArgs (TConstr "List" [ntp])

typeBindList :: [Bind]->State InferState [TBind]
typeBindList lst = do
 --oldCtx <- context <$> get
 storedBinds <- mapM storeBind lst
 -- Store binds in context before actually resolving, so values can have calls to one another, regardless of the order they were written in.
 typedBodies <- mapM typeBody lst
 zipWithM unifyWithPattern storedBinds typedBodies
 let ret = zipWith TBindVal storedBinds typedBodies
 -- Go back to old context before leaving
 -- If we add subcontexts, we have to uncomment the next line
 -- modify (\s -> s { context = oldCtx })
 return ret
 where
  typeBody :: Bind->State InferState [TValue]
  storeBind :: Bind->State InferState TPattern
  unifyWithPattern :: TPattern->[TValue]->State InferState ()
  unifyWithPattern ptn lst = do
   ptnType <- getType ptn
   lstTypes <- mapM getType lst
   foldM_ unify ptnType lstTypes
   return ()
  storeBind (BindVal name _) = do
   pat <- typePattern False name
   return pat
  typeBody (BindVal _ body) = do
   typedBody <- mapM typeVal body
   return typedBody

-- Infering the Actions Types
typeAction :: Action->State InferState TAction
typeAction (Assign ptrn vlus) = do
 tPtrn <- typePattern False ptrn
 tVlus <- mapM typeVal vlus
 onlyTPtrn <- getType tPtrn
 onlyTVlus <- mapM getType tVlus
 foldM_ unify onlyTPtrn onlyTVlus
 uPtrn <- unifyPattern tPtrn
 uVlus <- mapM unifyVal tVlus
 uPtrn' <- unifyPattern uPtrn
 uVlus' <- mapM unifyVal uVlus
 uPtrn'' <- unifyPattern uPtrn'
 uVlus'' <- mapM unifyVal uVlus'
 return $ TAssign uPtrn'' uVlus''
typeAction (Print vlus) = do
 tVlus <- mapM typeVal vlus
 -- TODO: handle strings better
 uVlus <- mapM unifyVal tVlus
 uVlus' <- mapM unifyVal uVlus
 mapM_ (getType >=> handleShow) uVlus'
 uVlus'' <- mapM unifyVal uVlus'
 return $ TPrint uVlus''
 where
  handleShow x = case x of
   TConstr "List" [TConstr "Char" []] -> pure x
   _ ->satisfy x ["Show"]
typeAction (Read rd) = do
 ctx <- gContext <$> get
 if Data.Map.member rd ctx then error ("Variable '"++rd++"' already exists") else do
  modify (\s->s{ gContext = Data.Map.insert rd (TConstr "List" [TConstr "Char" []]) ctx })
  return $ TRead rd
typeAction (ReadM t rd) = do
 ctx <- gContext <$> get
 if Data.Map.member rd ctx then error ("Variable '"++rd++"' already exists") else do
  modify (\s->s{ gContext = Data.Map.insert rd (TConstr "List" [TConstr "List" [TConstr "Char" []]]) ctx })
  return $ TReadM t rd

unifyPattern :: TPattern->State InferState TPattern
unifyPattern (TPatRef name typ) = do
 newTyp <- find typ
 return $ TPatRef name newTyp
unifyPattern x@(TPatLit _ _) = pure x
unifyPattern (TPatConstr name args tp) = do
 uArgs <- mapM unifyPattern args
 uTp <- find tp
 return $ TPatConstr name uArgs uTp
unifyPattern (TPatList lst tp) = do
 uArgs <- mapM unifyPattern lst
 uTp <- find tp
 return $ TPatList uArgs uTp

unifyVal :: TValue->State InferState TValue
unifyVal x@(TValLit _ _) = pure x
unifyVal (TValList lst typ) = do
 newLst <- mapM unifyVal lst
 newTyp <- find typ
 return $ TValList newLst newTyp
unifyVal (TValLambda args ret _ typ) = do
 newArgs <- mapM unifyPattern args
 newRet <- unifyVal ret
 newTyp <- find typ
 return $ TValLambda newArgs newRet [] newTyp
unifyVal (TValCall name args typ) = do
 newArgs <- mapM unifyVal args
 argTypes <- mapM getType newArgs
 newTyp <- (case typ of
  TInst par son->solve par son argTypes
  _-> find typ)
 return $ TValCall name newArgs newTyp
 where
  solve par son argTypes = do
   modify (\s -> s {specMap = mempty})
   newPar <- find par
   newSon <- find son
   specifiedFun <- specify newPar $ typeCall (argTypes++[newSon])
   let specifiedSon = destructFun specifiedFun argTypes
   modify (\s -> s {specMap = mempty})
   return $ TInst newPar specifiedSon
unifyVal (TValConstr name args typ) = do
 uArgs <- mapM unifyVal args
 uTp <- find typ
 return $ TValConstr name uArgs uTp

unifyBindList :: [TBind]->State InferState [TBind]
unifyBindList lst = mapM unifyBind lst
 where
  unifyBind (TBindVal name args) = do
   unifiedName <- unifyPattern name
   unifiedArgs <- mapM unifyVal args
   return $ TBindVal unifiedName unifiedArgs

-- Infering the binds types
typeBindGroup :: [Bind]->State InferState [TBind]
typeBindGroup bds = do
 initial <- typeBindList bds
 a <- unifyBindList initial
 b <- unifyBindList a
 c <- unifyBindList b
 d <- unifyBindList c
 e <- unifyBindList d
 f <- unifyBindList e
 return f

-- Function called from Main to infer types
typeProgram :: Program->TProgram
typeProgram (Program _ _ _ binds (Just actions)) = typed
 where
  ff@(!typed, !_) = runState runTypeInference initialState
  runTypeInference = do
   tBinds <- typeBindGroup binds
   tActions <- mapM typeAction actions
   return $ TProgram tBinds tActions

initialState :: InferState
initialState = execState (mapM checkBasicPolys (Prelude.map snd globalCtx)) $ InferState (Data.Set.fromList classes) mempty (Data.Map.fromList globalCtx) mempty 0 mempty
 where
  checkBasicPolys :: Type->State InferState Type
  checkBasicPolys (TVar x cns) = do
   modify (\s->s{store = Data.Map.insert x (TVar x cns) (store s)})
   vrc <- var_count <$> get
   if vrc <= x then modify (\s->s{var_count = x + 1}) else pure ()
   return $ TVar x cns
  checkBasicPolys (TConstr name lst) = liftM (TConstr name) (mapM checkBasicPolys lst)

tFun a b = TConstr "TFun" [a, b]
tInt = TConstr "Int" []
tBool = TConstr "Bool" []

tBinArith tp = tFun tp (tFun tp tp)
tComp tp = tFun tp $ tFun tp tBool
tString = TConstr "List" [TConstr "Char" []]

basicLib = [
 ("if", tFun tBool $ tFun (TVar 0 Data.Set.empty) $ tFun (TVar 0 Data.Set.empty) (TVar 0 Data.Set.empty)),
 ("++", tFun (TConstr "List" [TVar 1 Data.Set.empty]) $ tFun (TConstr "List" [TVar 1 Data.Set.empty]) (TConstr "List" [TVar 1 Data.Set.empty]))]

arithmetic = [
 ("+", tBinArith $ TVar 2 $ Data.Set.fromList ["Num"]),
 ("-", tBinArith $ TVar 3 $ Data.Set.fromList ["Num"]),
 ("*", tBinArith $ TVar 4 $ Data.Set.fromList ["Num"]),
 ("/", tBinArith $ TVar 5 $ Data.Set.fromList ["Num"]),
 ("%", tBinArith tInt),
 ("==", tComp $ TVar 7 $ Data.Set.fromList ["Ord"]),
 ("<", tComp $ TVar 8 $ Data.Set.fromList ["Ord"]),
 (">", tComp $ TVar 9 $ Data.Set.fromList ["Ord"]),
 ("<=", tComp $ TVar 10 $ Data.Set.fromList ["Ord"]),
 (">=", tComp $ TVar 11 $ Data.Set.fromList ["Ord"]),
 ("show", tFun (TVar 12 $ Data.Set.fromList ["Show"]) tString),
 ("parseInt", tFun tString tInt),
 ("parseDouble", tFun tString $ TConstr "Double" [])]

globalCtx = arithmetic ++ basicLib

classes = [
  ("Show", "Int"),
  ("Show", "Bool"),
  ("Show", "Char"),
  ("Show", "Double"),
  ("Num", "Double"),
  ("Num", "Int"),
  ("Ord", "Double"),
  ("Ord", "Int")]

