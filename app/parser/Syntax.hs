module Syntax where

newtype Program = Program [Declaration]
  deriving (Show)

type Identifier = String

data Declaration = Declaration Identifier Expression
  deriving (Eq, Ord, Show)

isAtomicExpression :: Expression -> Bool
isAtomicExpression (EConstant _) = True
isAtomicExpression (EIdent _) = True
isAtomicExpression (EConstr _) = True
isAtomicExpression _ = False

data Expression
  = EConstant Literal
  | EApp Expression Expression
  | ECase Expression [Alternative]
  | ELam Identifier Expression
  | EConstr Int
  | EIdent Identifier
  deriving (Eq, Ord, Show)

data Alternative = Alternative Identifier [Identifier] Expression
  deriving (Eq, Ord, Show)

data Literal
  = IntLit Int
  | RealLit Double
  | CharLit Char
  deriving (Eq, Ord, Show)
