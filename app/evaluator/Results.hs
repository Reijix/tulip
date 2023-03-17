module Results (showResults) where

import Evaluator
import qualified Data.Map.Strict as Map
import IRSyntax
import Data.List.Extra ( intercalate, headDef )

showResults :: String -> [GState] -> String
showResults "terse" = showResultsTerse
showResults _ = showResultsVerbose 

-- printing results terse
showResultsTerse :: [GState] -> String
showResultsTerse = showStack . last
    where 
        showStack s = "\n\nResult:\nStack[" ++ intercalate "\n" (map (showStackItem s) (reverse (gStack s))) ++ "]"
            where
            showStackItem s a = show node
                where node = (Map.!) (gHeap s) a

-- printing results verbose
iConcat :: [String] -> String
iConcat = concat

iDisplay = id

iNewline = "\n"
iStr = id
iInterleave :: String -> [String] -> String
iInterleave = intercalate
iLayn = concat
iIndent = id
iAppend = (++)
iNum = show
showAddr = show

showResultsVerbose :: [GState] -> String
showResultsVerbose states = iDisplay (iConcat [
    iStr "Supercombinator definitions", iNewline,
    iInterleave iNewline (map (showSC s) (Map.toList $ gGlobals s)),
    iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
    iLayn (map showState states)
    ])
    where (s:ss) = states

showSC :: GState -> (String, Addr) -> String
showSC s (name, addr) = iConcat [ iStr "Code for ", iStr name, iNewline,
                                  showInstructions code, iNewline, iNewline]
    where
        (NGlobal arity code) = (Map.!) (gHeap s) addr

showInstructions :: [Instruction] -> String
showInstructions is = iConcat [iStr "  Code:{",
                               iIndent (iInterleave iNewline (map showInstruction is)),
                               iStr "}", iNewline]
showInstruction :: Instruction -> String
showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "Pushglobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "Pushint " `iAppend` iNum n
showInstruction MkApp = iStr "Mkap"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
showInstruction (Alloc n) = iStr "Alloc " `iAppend` iNum n
showInstruction Eval = iStr "Eval"
showInstruction Add = iStr "Add"
showInstruction Sub = iStr "Sub"
showInstruction Mul = iStr "Mul"
showInstruction Div = iStr "Div"
showInstruction Eq = iStr "Eq"
showInstruction (Pack ident n2) = iConcat [iStr "Pack ", iStr ident, iStr " ", iNum n2]
showInstruction (Casejump ts) = iConcat [iStr "Casejump [", iIndent (iInterleave iNewline (map showCase ts)), iStr "]"]
    where showCase (tag, code) = iConcat [iStr tag, iStr ": ", shortShowInstructions 3 code]
showInstruction (Split n) = iStr "Split " `iAppend` iStr (show n)

showState :: GState -> String
showState s = iConcat [showStack s, iNewline,
                       showDump s, iNewline,
                       showInstructions (gCode s), iNewline]
 
showDump :: GState -> String
showDump s = iConcat [iStr " Dump:[", iIndent (iInterleave iNewline (map showDumpItem (reverse (gDump s)))), iStr "]"]

showDumpItem :: (GCode, GStack) -> String
showDumpItem (code, stack) = iConcat [iStr "<", shortShowInstructions 3 code, iStr ", ", shortShowStack stack, iStr ">"]

shortShowInstructions :: Int -> GCode -> String
shortShowInstructions number code = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
    where
        codes = map showInstruction (take number code)
        dotcodes = if length code > number
                    then codes ++ [iStr "..."]
                    else codes

shortShowStack :: GStack -> String
shortShowStack stack = iConcat [iStr "[", iInterleave (iStr ", ") (map (iStr . showAddr) stack), iStr "]"]

showStack :: GState -> String
showStack s = iConcat [iStr " Stack:[", iIndent (iInterleave iNewline (map (showStackItem s) (reverse (gStack s)))), iStr "]"]

showStackItem :: GState -> Addr -> String
showStackItem s a = iConcat [iStr (showAddr a), iStr ": ", showNode s a node]
    where node = (Map.!) (gHeap s) a

showNode :: GState -> Addr -> Node -> String
showNode s a (NInt n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
    where
        v = headDef (error "showNode: no head of globals with fitting name found") [n | (n,b) <- Map.toList $ gGlobals s, a == b]
showNode s a (NApp a1 a2) = iConcat [iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode s a (NInd a1) = iConcat [iStr "Ind ", iStr (showAddr a1)]
showNode s a (NData t as) = iConcat [iStr "Cons ", iStr t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]"]