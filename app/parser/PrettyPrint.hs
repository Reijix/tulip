module PrettyPrint (ppr) where

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Syntax

class PrettyPrint a where
  ppr :: a -> Text

pprIndent :: (PrettyPrint a) => Int -> a -> Text
pprIndent n str = Text.replicate n (pack " ") <> ppr str

pprParen :: (PrettyPrint a) => a -> Text
pprParen str = pack "(" <> ppr str <> pack ")"

instance PrettyPrint Program where
  ppr (Program decls) = Text.intercalate (pack ";\n") (map ppr decls) <> pack ";"

instance PrettyPrint Declaration where
  ppr (Declaration ident expr) = pack ident <> pack " = " <> ppr expr

instance PrettyPrint Expression where
  ppr (EConstant lit) = ppr lit
  ppr (EApp e1 e2) = exprParen e1 <> pack " " <> exprParen e2
    where
      exprParen e | isAtomicExpression e = ppr e
      exprParen e = pprParen e
  ppr (ECase e alts) = pack "case " <> ppr e <> pack " of\n" <> Text.intercalate (pack ",\n") (map (pprIndent 4) alts)
  ppr (ELam ident e) = pack "\\" <> pack ident <> pack ". " <> ppr e
  ppr (EConstr n) = pack "data " <> pack (show n)
  ppr (EIdent ident) = pack ident

instance PrettyPrint Literal where
  ppr (IntLit n) = pack $ show n
  ppr (RealLit n) = pack $ show n
  ppr (CharLit c) = pack [c]

instance PrettyPrint Alternative where
  ppr (Alternative ident args e) = pack ident <> unwordsWrap (map pack args) <> pack " -> " <> ppr e
    where 
      unwordsWrap [] = Text.empty
      unwordsWrap xs = pack " " <> Text.unwords xs