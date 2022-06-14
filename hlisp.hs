{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Lazy
import Data.Char
import Data.Functor
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Tuple
import System.Environment
import System.IO

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right s) = Right s
mapLeft f (Left x) = Left $ f x

newtype Parser a =
  Parser
    { runParser :: String -> Either String (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const (Left "Unsuccessfull parser by design")
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      case p1 input of
        (Left _) -> p2 input
        x -> x

mapError :: (String -> String) -> Parser a -> Parser a
mapError f (Parser p) = Parser (mapLeft f . p)

matchChar :: (Char -> Bool) -> Parser Char
matchChar pred =
  Parser $ \case
    (y:ys)
      | pred y -> Right (ys, y)
    (y:ys) ->
      Left
        ("Didn't match character '" ++
         [y] ++ "' with given predicate with remaining source: " ++ ys)
    [] -> Left "Didn't match character due to end of file"

charP :: Char -> Parser Char
charP x = mapError (("charP '" ++ [x] ++ "': ") ++) $ matchChar (== x)

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ Right . swap . span f

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Left "Expected input to be not null"
      else Right (input', xs)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = parseMany <|> pure []
  where
    parseMany = (:) <$> element <*> many (sep *> element)

(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
(.:) = (.) . (.)

data Instruction =
  Instruction

type Interpreter a = RWST () [Instruction] Env IO a

type Intrinsic = [Value] -> Interpreter Value

data Value
  = Nil
  | Number Int
  | String String
  | Symbol String
  | List [Value]
  | Intrinsic Intrinsic

boolToValue :: Bool -> Value
boolToValue False = Nil
boolToValue True = Symbol "t"

typeof :: Value -> String
typeof Nil = "nil"
typeof (Number _) = "number"
typeof (Symbol _) = "symbol"
typeof (String _) = "string"
typeof (List _) = "list"
typeof (Intrinsic _) = "intrinsic"

iterable :: Value -> Bool
iterable (String _) = True
iterable (List _) = True
iterable _ = False

intoList :: Value -> [Value]
intoList (List l) = l
intoList (String s) = Number . ord <$> s
intoList _ = error "intoList only accepts iterable Values"

applyList :: ([Value] -> Value) -> Value -> Value
applyList f xs
  | iterable xs = f $ intoList xs
applyList _ x = x

hasTypeNumber :: Value -> Bool
hasTypeNumber (Number n) = True
hasTypeNumber _ = False

maybeCoarceListToString :: [Value] -> Value
maybeCoarceListToString xs
  | all hasTypeNumber xs = String $ (\(Number n) -> chr n) <$> xs
maybeCoarceListToString xs = List xs

applyListCoarce :: ([Value] -> [Value]) -> Value -> Value
applyListCoarce f (List l) = List $ f l
applyListCoarce f (String s) = maybeCoarceListToString $ f $ Number . ord <$> s
applyListCoarce _ x = x

walk :: (Value -> Value) -> Value -> Value
walk f (List l) = List $ f <$> l
walk f (String s) = maybeCoarceListToString $ f . Number . ord <$> s
walk _ x = x

instance Show Value where
  show Nil = "nil"
  show (Number n) = show n
  show (Symbol s) = s
  show (String s) = show s
  show (List l) = "(" ++ unwords (map show l) ++ ")"
  show (Intrinsic _) = "<intrinsic>"

instance Eq Value where
  Nil == Nil = True
  (Number a) == (Number b) = a == b
  (Symbol a) == (Symbol b) = a == b
  (String a) == (String b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False

checkOrder :: (Ordering -> Bool) -> Value -> Value -> Bool
checkOrder f (Number a) (Number b) = f (a `compare` b)
checkOrder f (String a) (String b) = f (a `compare` b)
checkOrder f (Symbol a) (Symbol b) = f (a `compare` b)
checkOrder f Nil Nil = f EQ
checkOrder f (List a) (List b) = and $ zipWith (checkOrder f) a b
checkOrder _ _ _ = False

acceptOrder :: [Ordering] -> Ordering -> Bool
acceptOrder = flip elem

instance Num Value where
  (Number a) + (Number b) = Number $ a + b
  (String a) + (String b) = String $ a ++ b
  xs + y
    | iterable xs = walk (+ y) xs
  x + ys
    | iterable ys = walk (x +) ys
  x + y = error $ "Invalid types for '+': " ++ typeof x ++ " and " ++ typeof y
  (Number a) - (Number b) = Number $ a - b
  xs - y
    | iterable xs = walk (flip (-) y) xs
  x - ys
    | iterable ys = walk (x -) ys
  x - y = error $ "Invalid types for '-': " ++ typeof x ++ " and " ++ typeof y
  (Number a) * (Number b) = Number $ a * b
  xs * y
    | iterable xs = walk (* y) xs
  x * ys
    | iterable ys = walk (x *) ys
  x * y = error $ "Invalid types for '*': " ++ typeof x ++ " and " ++ typeof y
  abs (Number n) = Number $ abs n
  abs x = error $ "Invalid type for 'abs': " ++ typeof x
  signum (Number n) = Number $ signum n
  signum x = error $ "Invalid type for 'signum': " ++ typeof x
  fromInteger n = Number $ fromInteger n

parseSymbolOrNil :: Parser Value
parseSymbolOrNil = (toValue .: (:)) <$> matchChar isFront <*> spanP isRemaining

toValue "nil" = Nil
toValue s = Symbol s

isFront :: Char -> Bool
isFront x = isAlpha x || elem x "?!-_.,-*/<>=!"

isRemaining :: Char -> Bool
isRemaining x = isNumber x || isFront x

parseNumber :: Parser Value
parseNumber = Number . read <$> (ws *> notNull (spanP isDigit) <* ws)

parseString :: Parser Value
parseString = ws *> stringP "\"" *> stringLiteral <* stringP "\"" <* ws
  where
    stringLiteral :: Parser Value
    stringLiteral = String <$> spanP (/= '\"')

parseList :: Parser Value
parseList =
  List <$> (ws *> stringP "(" *> sepBy ws parseValue <* stringP ")" <* ws)

parseValue :: Parser Value
parseValue =
  ws *> foldr1 (<|>) [parseSymbolOrNil, parseNumber, parseString, parseList] <*
  ws

parseFile :: Parser Value
parseFile = (\body -> List (Symbol "do" : body)) <$> many parseValue

type Env = M.Map String Value

evalBinOp :: (Value -> Value -> Value) -> Value
evalBinOp f = Intrinsic $ \args -> forM args eval <&> foldl1 f

evalIf :: Value
evalIf =
  Intrinsic $ \(condition:ifTrue:ifFalse) -> do
    eval condition >>= \case
      Nil -> maybe (return Nil) eval (listToMaybe ifFalse)
      _ -> eval ifTrue

evalQuote :: Value
evalQuote = Intrinsic $ return . List

evalEval :: Value
evalEval =
  Intrinsic $ \args -> do
    if length args == 1
      then eval (head args) >>= eval
      else List <$> (traverse eval >=> traverse eval) args

evalSet :: Value
evalSet =
  Intrinsic $ \(name:value) ->
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
evalRepeat =
  adapt $ \case
    [x] -> List $ repeat x
    [] -> Nil
    xs -> List $ cycle xs

evalTake :: Value
evalTake =
  adapt $ \case
    ((Number n):xs:_)
      | iterable xs -> applyListCoarce (take n) xs
    _ -> error "invalid take call"

evalList :: Value
evalList = adapt List

evalTail :: Value
evalTail = each $ applyListCoarce tail

evalZipWith :: Value
evalZipWith =
  Intrinsic $
  traverse eval >=> \case
    [join, lhs, rhs]
      | iterable lhs && iterable rhs ->
        let lhs' = intoList lhs
            rhs' = intoList rhs
            zipped = zipWith (\l r -> List [join, l, r]) lhs' rhs'
         in (List <$>) $ traverse eval zipped
    _ -> error "invalid zip call"

index :: Int -> Value
index n =
  each $ \case
    xs
      | iterable xs -> intoList xs !! n
    _ -> error "index not supported for this type"

evalPrint :: Value
evalPrint =
  Intrinsic $
  traverse eval >=> \args -> do
    forM_ args (liftIO . print')
    return Nil
  where
    print' (String s) = putStrLn s
    print' x = print x

evalComparison :: [Ordering] -> Value
evalComparison orderings = adapt $ boolToValue . compare
  where
    order = acceptOrder orderings
    compare :: [Value] -> Bool
    compare (x:y:remaining) = checkOrder order x y && compare (y : remaining)
    compare _ = True

defaultEnvironment :: Env
defaultEnvironment =
  M.fromList
    [ ("*", evalBinOp (*))
    , ("+", evalBinOp (+))
    , ("-", evalBinOp (-))
    , ("<", evalComparison [LT])
    , ("<=", evalComparison [LT, EQ])
    , (">", evalComparison [GT])
    , (">=", evalComparison [GT, EQ])
    , ("=", evalComparison [EQ])
    , ("!=", evalComparison [GT, LT])
    , ("do", evalDo)
    , ("eval", evalEval)
    , ("fn", evalQuote)
    , ("fst", index 0)
    , ("if", evalIf)
    , ("list", evalList)
    , ("print", evalPrint)
    , ("quote", evalQuote)
    , ("repeat", evalRepeat)
    , ("set", evalSet)
    , ("snd", index 1)
    , ("tail", evalTail)
    , ("take", evalTake)
    , ("zip-with", evalZipWith)
    ]

eval :: Value -> Interpreter Value
eval (Symbol s) = do
  env <- get
  case M.lookup s env of
    Just v -> return v
    _ -> error $ "Undefined variable '" ++ s ++ "'"
eval (List l)
  | null l = return Nil
eval (List (v:args)) = do
  eval v >>= \case
    (Intrinsic f) -> do
      f args
    (List ((List parameters):body)) -> do
      args <- forM args eval
      let callParams =
            M.fromList $ zipWith (\(Symbol s) v -> (s, v)) parameters args
      withRWST
        (\r s -> (r, callParams `M.union` s))
        (foldM (\_ b -> eval b) Nil body)
    v -> error $ "not callable: " ++ typeof v
eval x = return x

runWithEnv :: Value -> Env -> IO (Value, Env)
runWithEnv v = ((\(a, s, w) -> (a, s)) <$>) . runRWST (eval v) ()

run :: Value -> IO Value
run v = do
  (v', s) <- runWithEnv v defaultEnvironment
  return v'

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

runExecutables :: Env -> [Exec] -> IO Env
runExecutables env ((Code code):continue) =
  case runParser parseFile code of
    Left err -> do
      putStrLn $ "[ERROR] Failed to parse with error: " ++ err
      return env
    Right (_, value) -> do
      (_, env) <- runWithEnv value env
      runExecutables env continue
runExecutables env ((File path):continue) = do
  file <- readFile path
  runExecutables env (Code file : continue)
runExecutables env [] = return env

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
  env <- runExecutables defaultEnvironment $ executables options
  when (null (executables options) || interactiveMode options) $ repl env

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  code <- getLine
  case runParser parseValue code of
    (Left err) -> do
      putStrLn ("[ERROR] Failed to parse due to error: " ++ err)
    (Right (_, value)) -> do
      (result, env) <- runWithEnv value env
      print result
      repl env
