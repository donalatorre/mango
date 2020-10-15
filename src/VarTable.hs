module VarTable  where
import DataTypes

varTable :: Program -> [(String, Int)]
varTable (Program typeDefList _ _ _ _) = typeDefVars typeDefList (getTypes typeDefList)

getTypes :: [TypeDef] -> [String]
getTypes [] = []
getTypes ((TypeDef (newType, _) _): t) = (newType: getTypes t)

typeDefVars :: [TypeDef] -> [String] -> [(String, Int)]
typeDefVars [] _ = []
typeDefVars  ((TypeDef _ typeSigList): t) newTypes = storeConstructors typeSigList newTypes ++ typeDefVars t newTypes

storeConstructors :: [TypeSig] -> [String] -> [(String, Int)]
storeConstructors [] _= []
storeConstructors ((TypeConstr constructor typeSigList): t) newTypes = [(constructor, (countPrimitiveTypes typeSigList newTypes))] ++ storeConstructors t newTypes

countPrimitiveTypes :: [TypeSig] -> [String] -> Int
countPrimitiveTypes [] _ = 0
countPrimitiveTypes ((TypeConstr primitiveType _): t) newTypes= 
	if elem primitiveType primitiveTypes || elem primitiveType newTypes
		then 1 + countPrimitiveTypes t newTypes
		else error ("Error, " ++ primitiveType ++ " is not a primitive type")