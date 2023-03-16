module IRSyntax where

import Syntax 

type Addr = Int
type Code = [Instruction]

data Node
    = NInt Int
    | NApp Addr Addr
    | NGlobal Int [Instruction]
    | NInd Addr
    | NData Int [Addr]
    deriving (Show)

data Instruction
    = PushInt Int
    | PushGlobal Identifier
    | Push Int
    | MkApp
    | Unwind
    | Update Int
    | Pack Int Int
    | Split Int
    | Casejump [(Identifier, Code)]
    | Slide Int
    | Eval
    | Alloc Int
    | Pop Int
    deriving (Show)
