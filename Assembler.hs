module Assembler where

import Evaluator
import Parser

assembly :: [Instruction] -> IO ()
assembly = print
