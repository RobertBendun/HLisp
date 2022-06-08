{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Tuple
import System.IO
import qualified Data.Map.Lazy as M
import System.Environment

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

matchChar :: (Char -> Bool) -> Parser Char
matchChar pred = Parser $ \case
  (y:ys) | pred y -> Just (ys, y)
  _ -> Nothing

charP :: Char -> Parser Char
charP x = matchChar (==x)

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ Just . swap . span f

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
  then Nothing
  else Just (input', xs)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = parseMany <|> pure []
  where
    parseMany = (:) <$> element <*> many (sep *> element)

(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
(.:) = (.) . (.)

type Intrinsic = [Value] -> StateT Env IO Value

data Value
  = Nil
  | Number Int
  | String String
  | Symbol String
  | List [Value]
  | Intrinsic Intrinsic

instance Show Value where
  show Nil = "nil"
  show (Number n) = show n
  show (Symbol s) = s
  show (String s) = show s
  show (List l) = "(" ++ unwords (map show l) ++ ")"
  show (Intrinsic _) = "<intrinsic>"

typeof :: Value -> String
typeof Nil = "nil"
typeof (Number _) = "number"
typeof (Symbol _) = "symbol"
typeof (String _) = "string"
typeof (List _) = "list"
typeof (Intrinsic _) = "intrinsic"

instance Num Value where
  (Number a) + (Number b) = Number $ a + b
  (String a) + (String b) = String $ a ++ b
  (List x) + y = List $ map (+ y) x
  x + (List y) = List $ map (x +) y
  x + y = error $ "Invalid types for '+': " ++ typeof x ++ " and " ++ typeof y

  (Number a) - (Number b) = Number $ a - b
  (List x) - y = List $ map (flip (-) y) x
  x - (List y) = List $ map (x -) y
  x - y = error $ "Invalid types for '-': " ++ typeof x ++ " and " ++ typeof y

  (Number a) * (Number b) = Number $ a * b
  (List x) * y = List $ map (+ y) x
  x * (List y) = List $ map (x +) y
  x * y = error $ "Invalid types for '*': " ++ typeof x ++ " and " ++ typeof y

  abs (Number n) = Number $ abs n
  abs x = error $ "Invalid type for 'abs': " ++ typeof x

  signum (Number n) = Number $ signum n
  signum x = error $ "Invalid type for 'signum': " ++ typeof x

  fromInteger n = Number $ fromInteger n

parseSymbolOrNil :: Parser Value
parseSymbolOrNil = (toValue .: (:)) <$> matchChar isFront <*> spanP isRemaining
  where
    toValue "nil" = Nil
    toValue s = Symbol s

    isFront :: Char -> Bool
    isFront x = isAlpha x || isSymbol x || elem x "?!_.,-*/"

    isRemaining :: Char -> Bool
    isRemaining x = isNumber x || isFront x

parseNumber :: Parser Value
parseNumber = Number . read <$> notNull (spanP isDigit)

parseString :: Parser Value
parseString = stringP "\"" *> stringLiteral <* stringP "\""
  where
    stringLiteral :: Parser Value
    stringLiteral = String <$> spanP (/= '\"')

parseList :: Parser Value
parseList = List <$> (stringP "(" *> elements <* stringP ")")
  where
    elements = sepBy sep parseValue
    sep = charP ' ' *> ws

parseValue :: Parser Value
parseValue = foldr1 (<|>) [parseSymbolOrNil, parseNumber, parseString, parseList]

type Env = M.Map String Value

evalBinOp :: (Value -> Value -> Value) -> Value
evalBinOp f = Intrinsic $ \args -> forM args eval <&> foldl1 f

evalIf :: Value
evalIf = Intrinsic $ \(condition:ifTrue:ifFalse) -> do
  eval condition >>= \case
    Nil -> maybe (return Nil) eval (listToMaybe ifFalse)
    _ -> eval ifTrue

evalQuote :: Value
evalQuote = Intrinsic $ return . List

evalEval :: Value
evalEval = Intrinsic $ \args -> do
  if length args == 1
  then eval (head args) >>= eval
  else List <$> (traverse eval >=> traverse eval) args

evalSet :: Value
evalSet = Intrinsic $ \(name:value) ->
  case name of
    (Symbol s) -> do
      value <- eval (head value)
      modify $ M.insert s value
      return value
    _ -> error "Cannot set!"

evalDo :: Value
evalDo = adapt last

adapt :: ([Value] -> Value) -> Value
adapt f = Intrinsic $ (f <$>) . traverse eval

each :: (Value -> Value) -> Value
each f = adapt (List . map f)

evalRepeat :: Value
evalRepeat = adapt $ \case
    [x] -> List $ repeat x
    []  -> Nil
    xs  -> List $ cycle xs

evalTake :: Value
evalTake = adapt $ \case
    ((Number n):(List l):_) -> List $ take n l
    _ -> error "invalid take call"

evalList :: Value
evalList = adapt List

evalTail :: Value
evalTail = each $ \case
  (List l) -> List $ tail l
  _ -> error "invalid tail call"

evalZipWith :: Value
evalZipWith = Intrinsic $ traverse eval >=> \case
  [join, List lhs, List rhs] -> do
    let zipped = zipWith (\l r -> List [join, l, r]) lhs rhs
    (List <$>) $ traverse eval zipped
  _ -> error "invalid zip call"

index :: Int -> Value
index n = each $ \case
  (List l) -> l !! n
  _ -> error "index not supported for this type"

defaultEnvironment :: Env
defaultEnvironment = M.fromList [ ("+", evalBinOp (+))
                                , ("*", evalBinOp (*))
                                , ("-", evalBinOp (-))
                                , ("if", evalIf)
                                , ("quote", evalQuote)
                                , ("eval", evalEval)
                                , ("fn", evalQuote)
                                , ("set", evalSet)
                                , ("do", evalDo)
                                , ("repeat", evalRepeat)
                                , ("take", evalTake)
                                , ("list", evalList)
                                , ("fst", index 0)
                                , ("snd", index 1)
                                , ("tail", evalTail)
                                , ("zip-with", evalZipWith)
                                ]

eval :: Value -> StateT Env IO Value
eval (Symbol s) = do
  env <- get
  case M.lookup s env of
    Just v -> return v
    _ -> error $ "Undefined variable '" ++ s ++ "'"
eval (List l) | null l = return Nil
eval (List (v:args)) = do
  eval v >>= \case
    (Intrinsic f) -> do
      f args
    (List ((List parameters):body)) -> do
      args <- forM args eval
      let callParams = M.fromList $ zipWith (\(Symbol s) v -> (s, v)) parameters args
      callerEnv <- get
      let bodyEnv = callParams `M.union` callerEnv
      put bodyEnv
      returnValues <- forM body eval
      put callerEnv
      return $ last returnValues


    v -> error $ "not callable: " ++ typeof v
eval x = return x

runWithEnv :: Value -> Env -> IO (Value, Env)
runWithEnv v = runStateT (eval v)

run :: Value -> IO Value
run v = do
  (v', s) <- runStateT (eval v) defaultEnvironment
  return v'

-- Intented for use in GHCI
execute :: String -> IO (Maybe Value)
execute code = do
  case runParser parseValue code of
    Nothing -> do
      putStrLn "[ERROR] Failed to parse!"
      return Nothing
    Just (_, value) -> do
      result <- run value
      return $ Just result

data Options = Options
  { sourceFiles :: [String]
  , interactiveMode :: Bool
  }

defaultOptions :: Options
defaultOptions = Options { sourceFiles = [], interactiveMode = False }

parseArguments :: IO Options
parseArguments = foldr go defaultOptions <$> getArgs
  where
    go :: String -> Options -> Options
    go = undefined


main :: IO ()
main = repl defaultEnvironment

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  code <- getLine
  case runParser parseValue code of
    Nothing -> do
      putStrLn "[ERROR] Failed to parse!"
    Just (_, value) -> do
      (result, env) <- runWithEnv value env
      print result
      repl env
