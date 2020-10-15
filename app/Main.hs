module Main (main) where
import System.Environment
import Lexer (mylex)
import VarTable
import Text.Show.Pretty (ppShow)
import qualified Control.Exception as E

data Data = Cns String Int

myint :: Int
myint = 3

compile :: String->String
compile input = case mylex input of
 Left err->("Compiling failed:\n" ++ err)
 Right val->("Compilation successful!\nResult:\n" ++ (ppShow val) ++ "\nVariable Table\n" ++ (ppShow (varTable val)))

main :: IO ()
main = do
 args <- getArgs
 s <- readFile $ args !! 0
 putStrLn (compile s)
