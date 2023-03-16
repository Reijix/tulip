module Compiler (compile) where

import qualified Data.Map.Strict as Map
import IRSyntax
import Syntax

type Environment = Map.Map Identifier Addr

compile :: Program -> [(Identifier, Int, Code)]
compile (Program decls) = map compileDeclaration decls

compileDeclaration :: Declaration -> (Identifier, Int, Code)
compileDeclaration (DataDeclaration name args) = (name, args, [])
compileDeclaration (FunctionDeclaration name args body) = (name, n, compileC body (Map.fromList $ zip args [0..]) ++ [Update n, Pop n, Unwind])
    where n = length args

compileC :: Expression -> Environment -> Code
compileC (EConstant (IntLit n)) env = [PushInt n]
compileC (EApp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkApp]
compileC (EIdent name) env = 
    case elemM of
        Nothing -> [PushGlobal name]
        Just var -> [Push var]
    where
        elemM = Map.lookup name env
compileC (ECase e alts) env = compileC e env ++ [Eval, Casejump (map (compileAlt env) alts)]

compileAlt :: Environment -> Alternative -> (Identifier, Code)
compileAlt env (Alternative tag names e) = (tag, [Split n] ++ compileC e env' ++ [Slide n])
    where 
        env' = Map.fromList (zip names [0..]) `Map.union` argOffset n env
        n = length names
argOffset :: Int -> Environment -> Environment
argOffset n = Map.map (+n)