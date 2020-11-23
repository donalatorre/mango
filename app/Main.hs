module Main (main) where
import System.Environment
import Lexer (mylex)
import VarTable
import Text.Show.Pretty (ppShow)
import qualified Control.Exception as E
import Typing
import Control.Monad.State.Lazy
import DataTypes
import Eval

compile :: String->Program
compile input = case mylex input of
 Left err->error("Compiling failed:\n" ++ err)
 Right val->val

main :: IO ()
main = do
 args <- getArgs
 let (x:xs) = args
 s <- readFile $ args !! 0
 let compiled = compile s
 let x = 1
 putStrLn ("=============================================================================================")
 putStrLn ("\nParsing Result:\n\n"++ (ppShow compiled))
 putStrLn ("=============================================================================================")
 putStrLn ("Parsing succeeded!\n")
 let typed = typeProgram compiled
 putStrLn ("\nTyping Result:\n\n"++ (ppShow typed))
 putStrLn ("=============================================================================================")
 putStrLn ("Typing succeded!\n")
 putStrLn ("\nRUNNING PROGRAM:")
 resolveProgram typed
