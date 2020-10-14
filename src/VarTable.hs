module VarTable  where
import DataTypes

varTable :: Program -> [(String, Int)]
varTable (Program typeDefList _ _ _ _) = typeDefVars(typeDefList)

typeDefVars :: [TypeDef] -> [(String, Int)]
typeDefVars [] = []
typeDefVars  ((TypeDef _ typeSigList): t)= storeConstructors(typeSigList) ++ typeDefVars(t)

storeConstructors :: [TypeSig] -> [(String, Int)]
storeConstructors [] = []
storeConstructors ((TypeConstr constructor attributes): t) = [(constructor, (length attributes))] ++ storeConstructors(t)