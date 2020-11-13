module Lexer (mylex) where
import Typing
import DataTypes
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import qualified Control.Exception as E
import Data.Char

myl = []

uno = (1: myl)
dos = ("str": myl)

blank = oneOf " \t\n"

nameChar :: Parser Char
nameChar = noneOf "'\"()[] ,\t\n"

parseWithFirst :: Parser Char->Parser String
parseWithFirst parseFirst = (:) <$> parseFirst <*> (many nameChar)

parseUpper :: Parser String
parseUpper = parseWithFirst upper

parseNonUpper :: Parser String
parseNonUpper = parseWithFirst (lookAhead (notFollowedBy $ upper <|> digit) >> nameChar)

singleApply :: Parser a->Parser b->Parser ((a->b->c)->c)
singleApply parseName parseArg = do
 char '(' >> many blank
 name <- parseName
 body <- many blank >> parseArg
 many blank >> char ')'
 return $ ($body) . ($name)

strictApply :: Parser a->Parser b->Parser ((a->[b]->c)->c)
strictApply parseName parseArg = singleApply parseName (sepEndBy1 parseArg $ many1 blank)

flexibleApply :: Parser a->Parser b->Parser ((a->[b]->c)->c)
flexibleApply parseName parseArg = try (singleApply parseName (sepEndBy parseArg $ many1 blank)) <|> noParen
 where
  noParen = do {name <- parseName; return $ ($[]) . ($name)}

abstractList :: Parser a->Parser[a]
abstractList parseItem = do
 char '[' >> many blank
 list <- (sepEndBy parseItem $ (char ',' >> skipMany blank))
 char ']'
 return list

parseProgram :: Program->Parser Program
parseProgram prog@(Program tdl cdl cil bl vll) = (try parseProgItem) <|> ((many blank >> eof) *> ( return prog ))
 where
  parseProgItem = do
   many blank
   item <- (parseProgTypeDef <|> parseProgClassDef <|> parseProgClassInst <|> parseProgBind <|> parseProgMain)
   parseProgram item
  parseProgTypeDef = try $ liftM (\td -> Program (td:tdl) cdl cil bl vll) parseTypeDef
  parseProgClassDef = try $ liftM (\cd -> Program tdl (cd:cdl) cil bl vll) parseClassDef
  parseProgClassInst = try $ liftM (\ci -> Program tdl cdl (ci:cil) bl vll) parseClassInst
  parseProgBind = try $ liftM (\b -> Program tdl cdl cil (b:bl) vll) parseBind
  parseProgMain = try $ liftM ($(\_ vl-> Program tdl cdl cil bl (vll++[vl]))) $ strictApply (string "print") parseValue

parseClassInst :: Parser ClassInst
parseClassInst = liftM ($ClassInst) $ strictApply parseInstHead parseBindVal
 where
  parseInstHead = string "instance" >> many1 blank >> (liftM ($(,)) $ strictApply parseUpper parseTypeCons)
  parseTypeCons = liftM ($TypeConstr) $ flexibleApply parseUpper parseTypeCons

parseTypeSig :: Parser TypeSig
parseTypeSig = (liftM ($TypeConstr) $ flexibleApply parseUpper parseTypeSig)
 <|> (liftM TypeList $ abstractList parseTypeSig) <|> (liftM TypeRef parseNonUpper)

parseTypeDef :: Parser TypeDef
parseTypeDef = liftM ($TypeDef) $ strictApply parseTypeName parseConstrucDef
 where
  parseTypeName = liftM ($(,)) $ string "data" >> many1 blank >> flexibleApply parseUpper parseNonUpper
  parseConstrucDef = (liftM ($TypeConstr) $ flexibleApply parseUpper parseTypeSig)

parseClassDef :: Parser ClassDef
parseClassDef = liftM ($ClassDef) $ strictApply parseHead parseFunc
 where
  parseFunc = liftM ($(,)) $ singleApply (string "::" >> many1 blank >> parseNonUpper) parseTypeSig
  parseHead = string "class" >> many1 blank >> (liftM ($(,)) $ strictApply parseUpper parseNonUpper)

parsePattern :: Parser Pattern
parsePattern = parseConstr <|> parseList <|> parseLit <|> (liftM PatRef parseNonUpper)
 where
  parseConstr = liftM ($PatConstr) $ flexibleApply parseUpper parsePattern
  parseList = liftM PatList $ abstractList parsePattern
  parseLit = liftM PatLit $ parsePrim

parseValue :: Parser Value
parseValue = (try parseConstr) <|> parseList <|> (try parseLambda) <|> parseLit <|> parseCall
 where
  parseLit = liftM ValLit parsePrim
  parseConstr = liftM ($ValConstr) $ flexibleApply parseUpper parseValue
  parseList = liftM ValList $ abstractList parseValue
  parseCall = liftM ($ValCall) $ flexibleApply parseNonUpper parseValue
parseLambda = liftM ($ValLambda) $ flexibleApply parseLmbHead parseBind
parseLmbHead = do
 char '\\' >> many1 blank
 args <- (abstractList parsePattern)
 body <- (many1 blank >> parseValue)
 return (args, body)

parseBindVal :: Parser Bind
parseBindVal = liftM ($BindVal) $ strictApply (string "let" >> many1 blank >> parseNonUpper) parseValue

parseBind :: Parser Bind
parseBind = (try parseBindVal) <|> parseBindType
 where
  parseConstraint = liftM ($(,)) $ strictApply parseUpper parseNonUpper
  parseBindType = liftM ($BindType) $ flexibleApply parseTypeHead parseConstraint
  parseTypeHead = do
   name <- (string "::" >> many1 blank >> parseNonUpper)
   sign <- (many1 blank >> parseTypeSig)
   return (name, sign)

parsePrim :: Parser Prim
parsePrim = parseString <|> parseBool <|> parseChar <|> (try parseDouble) <|> parseInteger
 where
  parseString = do
   char '"'
   x <- many (noneOf "\"")
   char '"'
   return $ PString x
  parseInteger = liftM (PInt . read) $ many1 digit
  parseChar = do {string "'"; ret <- letter; string "'"; return $ PChar ret}
  parseDouble = do
   intg <- many1 digit
   char '.'
   decm <- many1 digit
   return $ (PDouble . read) $ intg ++ "." ++ decm
  parseBool = parseTrue <|> parseFalse
  parseTrue = do {string "True"; return $ PBool True}
  parseFalse = do {string "False"; return $ PBool False}

prsl :: Parser (String, [String])
prsl = liftM ($(,)) $ flexibleApply parseUpper parseUpper

mylex :: String->(Either String Program)
mylex input =
 case parse (many blank >> (parseProgram $ Program [] [] [] [] [])) "lexer" input of
 Left err->Left $ "Lexing error:\n" ++ (show err)
 Right val-> Right val
