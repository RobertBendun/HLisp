{-# LANGUAGE LambdaCase #-}

module Evaluator where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Lazy
import Data.Functor
import qualified Data.Map.Lazy as M
import Data.Maybe
import Parser
import System.Environment
import System.IO

evalBinOp :: (Value -> Value -> Value) -> Value
evalBinOp f = Intrinsic $ \args -> forM args eval <&> foldl1 f

evalIf :: Value
evalIf =
  Intrinsic $ \(condition:ifTrue:ifFalse) -> do
    eval condition >>= \case
      Nil -> maybe (return Nil) eval (listToMaybe ifFalse)
      _ -> eval ifTrue

evalQuote :: Value
evalQuote =
  Intrinsic $ \case
    [x] -> return x
    err -> error $ "Invalid arguments for quote: " ++ show err

evalFn :: Value
evalFn = Intrinsic $ return . List

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

evalLabel :: Value
evalLabel =
  Intrinsic $
  traverse eval >=> \args -> do
    tell $ (\(Symbol s) -> Label s) <$> args
    return Nil

evalAsmCall :: String -> Value
evalAsmCall name =
  Intrinsic $
  traverse eval >=> \args -> do
    tell [Call (Symbol name : args)]
    return Nil

evalDb :: Value
evalDb =
  Intrinsic $
  traverse eval >=> \args -> do
    tell [Call (Symbol "db" : args)]
    return Nil

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
    , ("fn", evalFn)
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
    -- assembler
    , ("label", evalLabel)
    , ("db", evalDb)
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
      ctx <- get
      put $ callParams `M.union` ctx
      result <- foldM (\_ b -> eval b) Nil body
      put ctx
      return result
    v -> error $ "not callable: " ++ typeof v
eval x = return x
