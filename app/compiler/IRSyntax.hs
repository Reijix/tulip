module IRSyntax where

import Syntax 

type Addr = Int
type Code = [Instruction]

data Instruction
    = PushInt Int
    | PushGlobal Identifier
    | Push Int
    | MkApp
    | Unwind
    | Update Int
    | Pack Identifier Int
    | Split Int
    | Casejump [(Identifier, Code)]
    | Slide Int
    | Eval
    | Alloc Int
    | Pop Int
    | Add | Sub | Mul | Div
    deriving (Show)
