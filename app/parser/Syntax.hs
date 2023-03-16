module Syntax where

newtype Program = Program [Declaration]
  deriving (Show)

type Identifier = String

data Declaration 
  = FunctionDeclaration Identifier [Identifier] Expression
  | DataDeclaration Identifier Int
  deriving (Eq, Ord, Show)

isAtomicExpression :: Expression -> Bool
isAtomicExpression (EConstant _) = True
isAtomicExpression (EIdent _) = True
isAtomicExpression _ = False

data Expression
  = EConstant Literal
  | EApp Expression Expression
  | ECase Expression [Alternative]
  | EIdent Identifier
  deriving (Eq, Ord, Show)

data Alternative = Alternative Identifier [Identifier] Expression
  deriving (Eq, Ord, Show)

data Literal
  = IntLit Int
  | RealLit Double
  | CharLit Char
  deriving (Eq, Ord, Show)
