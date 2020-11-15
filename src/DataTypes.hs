module DataTypes where

primitiveTypes :: [String]
primitiveTypes = ["Int", "String", "Double", "Char"]

data TypeSig = TypeRef String | TypeList [TypeSig] | TypeConstr String [TypeSig] deriving (Show)
data TypeDef = TypeDef (String, [String]) [TypeSig] deriving (Show)
data ClassDef = ClassDef (String, [String]) [(String, TypeSig)] deriving (Show)
data ClassInst = ClassInst (String, [TypeSig]) [Bind] deriving (Show)
data Prim = PBool Bool | PInt Int | PDouble Double | PString String | PChar Char deriving (Show)
data Bind = BindVal String [Value] | BindType (String, TypeSig) [(String, [String])] deriving (Show) -- TODO: enable type constraints
data Pattern = PatLit Prim | PatConstr String [Pattern] | PatList [Pattern] | PatRef String deriving (Show)
data Value = ValLit Prim
 | ValConstr String [Value]
 | ValCall String [Value]
 | ValList [Value]
 | ValLambda ([Pattern], Value) [Bind] deriving (Show)

data Program = Program [TypeDef] [ClassDef] [ClassInst] [Bind] [[Value]] deriving (Show) -- Value list is for prints

