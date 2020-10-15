module VarTable  where
import DataTypes

varTable :: Program -> [(String, Int)]
varTable (Program typeDefList _ _ _ _) = typeDefVars typeDefList

typeDefVars :: [TypeDef] -> [(String, Int)]
typeDefVars [] = []
typeDefVars  ((TypeDef _ typeSigList): t)= storeConstructors typeSigList ++ typeDefVars t

storeConstructors :: [TypeSig] -> [(String, Int)]
storeConstructors [] = []
storeConstructors ((TypeConstr constructor typeSigList): t) = [(constructor, (countPrimitiveTypes typeSigList))] ++ storeConstructors t

countPrimitiveTypes :: [TypeSig] -> Int
countPrimitiveTypes [] = 0
countPrimitiveTypes ((TypeConstr primitiveType _): t) = 
	if elem primitiveType primitiveTypes
		then 1 + countPrimitiveTypes t
		else error ("Error, " ++ primitiveType ++ " is not a primitive type")