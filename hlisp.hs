module Main
  ( main
  ) where

import Assembler
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Lazy
import qualified Data.Map.Lazy as M
import Evaluator
import Parser
import System.Environment
import System.IO

data Exec
  = File String
  | Code String

data Options =
  Options
    { executables :: [Exec]
    , interactiveMode :: Bool
    }

appendExecutable :: Options -> Exec -> Options
appendExecutable opt exec = opt {executables = executables opt ++ [exec]}

runExecutables :: [Exec] -> Interpreter Value
runExecutables ((Code code):continue) =
  case runParser parseFile code of
    Left err -> do
      (lift . putStrLn) $ "[ERROR] Failed to parse with error: " ++ err
      return Nil
    Right (_, value) -> do
      eval value
      runExecutables continue
runExecutables ((File path):continue) = do
  file <- (lift . readFile) path
  runExecutables (Code file : continue)
runExecutables [] = return Nil

defaultOptions :: Options
defaultOptions = Options {executables = [], interactiveMode = False}

parseArguments :: IO Options
parseArguments = snd . go defaultOptions <$> getArgs
  where
    go :: Options -> [String] -> ([String], Options)
    go opt ("--repl":remaining) = (remaining, opt {interactiveMode = True})
    go opt (param:code:remaining)
      | param `elem` ["-c", "--run"] =
        (remaining, appendExecutable opt $ Code code)
    go opt (path:remaining) = (remaining, appendExecutable opt $ File path)
    go opt [] = ([], opt)

main :: IO ()
main = do
  options <- parseArguments
  (_, _, instructions) <-
    (\m -> runRWST m () defaultEnvironment) $ do
      runExecutables $ executables options
      when (null (executables options) || interactiveMode options) repl
  unless (null instructions) $ assembly instructions

repl :: Interpreter ()
repl = do
  (lift . putStr) "> "
  (lift . hFlush) stdout
  code <- lift getLine
  case runParser parseValue code of
    (Left err) -> do
      (lift . putStrLn) ("[ERROR] Failed to parse due to error: " ++ err)
    (Right (_, value)) -> do
      result <- eval value
      (lift . print) result
      repl
