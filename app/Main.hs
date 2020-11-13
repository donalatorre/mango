module Main (main) where
import System.Environment
import Lexer (mylex)
import VarTable
import Text.Show.Pretty (ppShow)
import qualified Control.Exception as E
import Typing
import Control.Monad.State.Lazy
import DataTypes

compile :: String->Program
compile input = case mylex input of
 Left err->error("Compiling failed:\n" ++ err)
 Right val->val

main :: IO ()
main = do
 args <- getArgs
 s <- readFile $ args !! 0
 let compiled = compile s
 putStrLn ("=============================================================================================")
 putStrLn ("\nParsing successful!\nResult:\n\n"++ (ppShow compiled))
 let (Program _ _ _ bds _) = compiled
 let (typed, _) = runState (runTypeInference bds) initialState
 putStrLn ("=============================================================================================")
 putStrLn ("\nTyping successful!\nResult:\n\n"++ (ppShow typed))
 putStrLn ("=============================================================================================")
