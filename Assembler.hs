{-# LANGUAGE BinaryLiterals, LambdaCase #-}

module Assembler where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import Data.Foldable (find)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Word
import Evaluator (evalAsmCall)
import Parser

data Operand
  = Register8
  | Register16
  | Register32
  | Register64
  | Immidiate8
  | Immidiate16
  | Immidiate32
  | Immidiate64
  | Immidiate
  deriving (Show, Eq)

match ::
     Operand -- required operand
  -> Operand -- received operand
  -> Bool
match Immidiate op = isImmidiate op
match op Immidiate = isImmidiate op
match a b = a == b

isRegister :: Operand -> Bool
isRegister Register8 = True
isRegister Register16 = True
isRegister Register32 = True
isRegister Register64 = True
isRegister _ = False

isImmidiate :: Operand -> Bool
isImmidiate Immidiate8 = True
isImmidiate Immidiate16 = True
isImmidiate Immidiate32 = True
isImmidiate Immidiate64 = True
isImmidiate Immidiate = True
isImmidiate _ = False

newtype Factory a =
  Factory
    { runFactory :: [(Operand, Int)] -> Either String a
    }

instance Semigroup a => Semigroup (Factory a) where
  (Factory a) <> (Factory b) =
    Factory $ \input -> a input >>= (\v -> (v <>) <$> b input)

constant :: [Word8] -> Factory B.ByteString
constant bytes = Factory $ Right . const (B.pack bytes)

addRegister :: Word8 -> Int -> Factory B.ByteString
addRegister base index =
  Factory $ \case
    xs
      | length xs > index && isRegister (fst (xs !! index)) ->
        Right $ B.singleton $ base + toEnum (snd (xs !! index))
    _ -> Left "addRegister: failed"

immmidiate :: Int -> Factory B.ByteString
immmidiate index =
  Factory $ \case
    xs
      | length xs > index ->
        case xs !! index of
          (Immidiate8, v) -> Right $ toLazyByteString $ int8 $ toEnum v
          (Immidiate16, v) -> Right $ toLazyByteString $ int16LE $ toEnum v
          (Immidiate32, v) -> Right $ toLazyByteString $ int32LE $ toEnum v
          (Immidiate64, v) -> Right $ toLazyByteString $ int64LE $ toEnum v
          _ -> Left "immmidiate: invalid operand type"
    _ -> Left "immmidiate: not enough arguments"

data InstructionTemplate =
  InstructionTemplate
    { name :: String
    , shape :: [Operand]
    , bytes :: Factory B.ByteString
    }

instructions :: [InstructionTemplate]
instructions =
  [ InstructionTemplate
      {name = "syscall", shape = [], bytes = constant [0x0f, 0x05]}
  , InstructionTemplate
      { name = "mov"
      , shape = [Register32, Immidiate32]
      , bytes = addRegister 0xb8 0 <> immmidiate 1
      }
  ]

instructionsEnv :: Env
instructionsEnv =
  M.mapWithKey (\k _ -> evalAsmCall k) $
  M.fromList $ zip (map name instructions) (repeat ())

data Instruction =
  Instruction
    { args :: [(Operand, Int)]
    , template :: InstructionTemplate
    }

runInstruction :: Instruction -> Either String B.ByteString
runInstruction instr = runFactory (bytes (template instr)) $ args instr

instance Show Instruction where
  show instr = "{ " ++ name (template instr) ++ " }"

fromRegisterName :: String -> Maybe (Operand, Int)
fromRegisterName "rax" = Just (Register64, 0)
fromRegisterName "eax" = Just (Register32, 0)
fromRegisterName "ax" = Just (Register16, 0)
fromRegisterName "al" = Just (Register8, 0)
fromRegisterName "rdi" = Just (Register64, 7)
fromRegisterName "edi" = Just (Register32, 7)
fromRegisterName _ = Nothing

isRegisterName :: String -> Bool
isRegisterName = isJust . fromRegisterName

toArg :: Value -> (Operand, Int)
toArg (Symbol s)
  | isRegisterName s =
    case fromRegisterName s of
      Just x -> x
      Nothing -> error "unreachable due to isRegisterName"
toArg (Number n) = (Immidiate, n)
toArg v = error $ "invalid toArg invocation: " ++ show v

toInstruction :: [Value] -> Maybe Instruction
toInstruction ((Symbol s):params) = fill <$> find check instructions
  where
    args = map toArg params
    check :: InstructionTemplate -> Bool
    check tmpl =
      (length args == length (shape tmpl)) &&
      and (zipWith match (map fst args) (shape tmpl))
    fill :: InstructionTemplate -> Instruction
    fill tmpl =
      Instruction
        {args = zipWith (\a b -> (b, snd a)) args (shape tmpl), template = tmpl}
toInstruction _ = error "invalid toInstruction call"

assembly :: [Statement] -> IO Builder
assembly (Call params:xs) =
  case toInstruction params of
    Just instruction -> do
      case runInstruction instruction of
        (Right bytes) -> do
          rest <- assembly xs
          return $ lazyByteString bytes <> rest
        (Left s) -> do
          putStrLn $ "Failed to assemble: " ++ s
          return mempty
    Nothing -> do
      putStrLn $ "Failed to assemble: " ++ show params
      return mempty
assembly (Label l:xs) = undefined
assembly [] = return mempty
