{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Data.Char
import qualified Data.Map.Lazy as M
import Data.Tuple

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

data Instruction
  = Label String
  | Mov Value Value
  | Syscall
  | DefineBytes [Value]
  deriving (Show)

type Env = M.Map String Value

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

parseQuote :: Parser Value
parseQuote =
  (\body -> List [Symbol "quote", body]) <$> (charP '\'' *> parseValue)

parseValue :: Parser Value
parseValue =
  ws *>
  foldr1
    (<|>)
    [parseSymbolOrNil, parseNumber, parseString, parseList, parseQuote] <*
  ws

parseFile :: Parser Value
parseFile = (\body -> List (Symbol "do" : body)) <$> many parseValue
