module Typing where

import DataTypes
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Map
import Control.Monad

data TBind = TBindVal TPattern [TValue] deriving (Show) -- TODO: enable type constraints
data TPattern = TPatLit Prim Type | TPatConstr String [TPattern] Type | TPatList [Pattern] Type | TPatRef String Type deriving (Show)
data TValue = TValLit Prim Type
 | TValConstr String [TValue] Type
 | TValCall String [TValue] Type
 | TValList [TValue] Type
 | TValLambda [TPattern] TValue [TBind] Type deriving (Show)
data TAction = TAssign TPattern [TValue] | TPrint [TValue] | TRead String deriving (Show)
data TProgram = TProgram [TBind] [TAction] deriving (Show)

data Type = TConstr String [Type] | TVar Int | TArg Int | TInst Type Type deriving (Show)


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
  store :: Map Int Type,
  context :: Map String Type,
  var_count :: Int,
  --TODO: change into multiple state
  specMap :: Map Int Type
} deriving (Show)

runConstr (TConstr a args) (TConstr b brgs) fun = if a /= b then error ("Couldn't unify '"++a++"'"++" with '"++b++"'") else checkedResult
 where
  checkedResult = if (length args) == (length brgs) then result else error "Constructors don't have the same number of arguments"
  result = do
   lst <- fun args brgs
   return $ TConstr a lst

unify :: Type->Type->State InferState Type
unify a b = do
 prntA <- find a
 prntB <- find b
 unified <- unify' a b
 return unified
 where
  unify' :: Type->Type->State InferState Type
  unify' x@(TConstr _ _) y@(TConstr _ _) = runConstr x y (zipWithM unify')
  unify' a@(TConstr _ _) b@(TVar _) = unify' b a
  unify' (TVar x) b = do
   modify (\s -> s { store = insert x b (store s) })
   return b

find :: Type->State InferState Type

find (TArg id) = do
 found <- find $ TVar id
 let (TVar foundId) = found
 return $ TArg foundId

find (TConstr nm args) = do
 evld <- mapM find args
 return $ TConstr nm evld

find (TVar x) = do
 str <- store <$> get
 let y = fromJust $ Data.Map.lookup x str
 newY <- case y of 
  TVar x -> pure y
  _ -> find y
 modify (\s -> s { store = insert x newY str })
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
  specify' con@(TConstr a args) (TVar x) = do
   brgs <- replicateM (length args) newMetaVar
   res <- zipWithM specify' args brgs
   let newTp = (TConstr a res)
   modify (\s -> s { store = insert x newTp (store s) })
   return newTp
  specify' (TVar x) b = do
   spec <- specMap <$> get
   let matched = Data.Map.lookup x spec
   ret <- if isJust matched then unify b (fromJust matched) else (modify (\s -> s {specMap = insert x b spec})) >> pure b
   return ret

newMetaVar :: State InferState Type
newMetaVar = do
 v <- var_count <$> get
 modify (\s -> s {store = insert v (TVar v) (store s), var_count = v + 1})
 return $ TVar v

destructFun :: Type->[Type]->Type
destructFun typ [] = typ
destructFun (TConstr "TFun" [_, body]) (_: rest) = destructFun body rest

typeCall :: [Type]->Type
typeCall [] = error "Empty list"
typeCall [x] = x
typeCall (x: xs) = TConstr "TFun" [x, typeCall xs]

typeVal :: Value->State InferState TValue
typeVal (ValLit prm) = pure $ TValLit prm $ TConstr (case prm of
 PBool _ -> "Bool"
 PInt _ -> "Int"
 PDouble _ -> "Double"
 PString _ -> "String"
 PChar _ -> "Char") []

typeVal (ValCall name args) = do
 ctx <- context <$> get
 let rawPrnt = Data.Map.lookup name ctx
 let prnt = if isNothing rawPrnt then error ("Variable '"++name++"' not in scope.") else rawPrnt
 let (Just prntTyp) = prnt
 ret <- newMetaVar
 typedArgs <- mapM typeVal args
 finalPrntTyp <- case prntTyp of
  TArg id-> getFuncType typedArgs ret (TVar id)
  _->pure $ TInst prntTyp ret
 return $ TValCall name typedArgs finalPrntTyp
 where
  getFuncType typedArgs ret vr = do
   onlyTypes <- mapM getType typedArgs
   let funcd = typeCall (onlyTypes++[ret])
   unified <- unify vr funcd
   return $ destructFun unified onlyTypes

typeVal (ValLambda (args, ret) _) = do
 oldCtx <- context <$> get
 typedArgs <- mapM (typePattern True) args
 onlyTypes <- mapM getType typedArgs
 typedRet <- typeVal ret
 onlyRetType <- getType typedRet
 let funcTyp = arrow onlyTypes onlyRetType
 -- Go back to original context
 modify (\s -> s {context = oldCtx})
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

typePattern :: Bool->Pattern->State InferState TPattern
typePattern isArg (PatRef name) = do
 ctx <- context <$> get
 if member name ctx then error ("Variable '"++name++"' already exists.") else do
   vr <- newMetaVar
   let (TVar id) = vr
   modify (\s -> s { context = insert name (if isArg then TArg id else vr) ctx })
   return $ TPatRef name vr

typeBindList :: [Bind]->State InferState [TBind]
typeBindList lst = do
 oldCtx <- context <$> get
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
   -- TODO: change name from string to pattern
   pat <- typePattern False name
   return pat
  typeBody (BindVal _ body) = do
   typedBody <- mapM typeVal body
   return typedBody

typeAction :: Action->State InferState TAction
typeAction (Assign ptrn vlus) = do
 tPtrn <- ((typePattern True) >=> unifyPattern) ptrn
 tVlus <- mapM (typeVal >=> unifyVal) vlus
 return $ TAssign tPtrn tVlus
typeAction (Print vlus) = do
 tVlus <- mapM (typeVal >=> unifyVal) vlus
 return $ TPrint tVlus
typeAction (Read rd) = do
 ctx <- context <$> get
 if Data.Map.member rd ctx then error ("Variable '"++rd++"' already exists") else do
  modify (\s->s{ context = insert rd (TConstr "String" []) (context s) })
  return $ TRead rd

unifyPattern :: TPattern->State InferState TPattern
unifyPattern (TPatRef name typ) = do
 newTyp <- find typ
 return $ TPatRef name newTyp

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
   modify (\s -> s {specMap = empty})
   newPar <- find par
   newSon <- find son
   specifiedFun <- specify newPar $ typeCall (argTypes++[newSon])
   let specifiedSon = destructFun specifiedFun argTypes
   modify (\s -> s {specMap = empty})
   return $ TInst newPar specifiedSon

unifyBindList :: [TBind]->State InferState [TBind]
unifyBindList lst = mapM unifyBind lst
 where
  unifyBind (TBindVal name args) = do
   unifiedName <- unifyPattern name
   unifiedArgs <- mapM unifyVal args
   return $ TBindVal unifiedName unifiedArgs

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

typeProgram :: Program->TProgram
typeProgram (Program _ _ _ binds (Just actions)) = typed
 where
  (typed, _) = runState runTypeInference initialState
  runTypeInference = do
   tBinds <- typeBindGroup binds
   tActions <- mapM typeAction actions
   return $ TProgram tBinds tActions

initialState :: InferState
initialState = InferState (fromList [(0, TVar 0)]) (fromList globalCtx) 1 empty

headTyp = TConstr "TFun" [TConstr "List" [TVar 0], TVar 0]
headCall = TConstr "TFun" [TConstr "List" [TVar 1], TVar 2]
headStore = InferState (fromList [(0, TVar 0), (1, TVar 1), (2, TVar 2)]) empty 0 empty

otrStore = InferState (fromList [(0, TVar 0), (1, TVar 1), (2, TVar 2)]) empty 0 $ fromList [(0, TVar 1)]
otrTyp = TVar 0
otrCall = TVar 2
bv = BindVal (PatRef "x") [ ValLit (PInt 3) ]

myCall = (ValCall "+" [ValLit $ PInt 2, ValLit $ PInt 3])
myLamb = ValLambda ([PatRef "a", PatRef "b"], ValCall "c" []) []
--vlu wh init = runState (typeVal wh) init

tFun a b = TConstr "TFun" [a, b]
tBool = TConstr "Bool" []
tInt = TConstr "Int" []
tDouble = TConstr "Double" []
tString = TConstr "String" []


tBinArith tp = tFun tp (tFun tp tp)

arithmetic = [
 ("+", tBinArith tInt),
 ("-", tBinArith tInt),
 ("*", tBinArith tInt),
 ("/", tBinArith tInt)]

basicLib = [
 ("if", tFun tBool $ tFun (TVar 0) $ tFun (TVar 0) (TVar 0)),
 ("++", tFun tString $ tFun tString tString)]

globalCtx = arithmetic ++ basicLib
