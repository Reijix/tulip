module Evaluator where

import IRSyntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char ( digitToInt )
import Data.List.Extra (headDef, mapAccumL)
import Control.Monad.State.Strict
import Syntax

-- heap nodes
data Node
    = NInt Int
    | NApp Addr Addr
    | NGlobal Int [Instruction]
    | NInd Addr
    | NData Identifier [Addr]
    deriving (Show)

type GCode = Code
type GStack = [Addr]
type GDump = [(GCode, GStack)]
type GHeap = Map Addr Node
hNull :: Addr
hNull = -1
hAlloc :: Map Addr Node -> Node -> (Map Addr Node, Addr)
hAlloc heap node = (Map.insert a node heap, a)
    where a = Map.size heap
type GGlobals = Map Identifier Addr

data GState = GState {
    gCode :: GCode,
    gStack :: GStack,
    gDump :: GDump,
    gHeap :: GHeap,
    gGlobals :: GGlobals
    }
    deriving (Show)

type GMonad = State GState
-- getter / setter for state
getCode :: GMonad [Instruction]
getCode = gets gCode
putCode :: [Instruction] -> GMonad ()
putCode code = modify (\state -> state {gCode = code})
getStack :: GMonad [Addr]
getStack = gets gStack
putStack :: [Addr] -> GMonad ()
putStack stack = modify (\state -> state {gStack = stack})
getDump :: GMonad GDump
getDump = gets gDump
putDump :: GDump -> GMonad ()
putDump dump = modify (\state -> state {gDump = dump})
getHeap :: GMonad (Map Addr Node)
getHeap = gets gHeap
putHeap :: Map Addr Node -> GMonad ()
putHeap heap = modify (\state -> state {gHeap = heap})
getGlobals :: GMonad (Map Identifier Addr)
getGlobals = gets gGlobals
putGlobals :: Map Identifier Addr -> GMonad ()
putGlobals globals = modify (\state -> state {gGlobals = globals})

gFinal :: GMonad Bool
gFinal = null <$> getCode

step :: GMonad ()
step = do
    is <- getCode
    putCode $ tail is
    dispatch $ headDef (error "step: no instruction, head failed") is

dispatch :: Instruction -> GMonad ()
dispatch (PushGlobal f) = pushglobal f
dispatch (PushInt n) = pushint n
dispatch MkApp = mkap
dispatch (Push n) = push n
dispatch Unwind = unwind
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Slide n) = slide n
dispatch (Alloc n) = alloc n
dispatch Eval = doEval
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch (Pack t as) = pack t as
dispatch (Casejump ts) = casejump ts
dispatch (Split n) = split n

rearrange :: Int -> GMonad ()
rearrange n = do
    heap <- getHeap
    stack <- getStack
    let as' = map (getArg . (Map.!) heap) (tail stack)
    putStack $ take n as' ++ drop n stack

-- instructions
pushglobal :: Identifier -> GMonad ()
pushglobal name@['P', 'a', 'c', 'k', '{', t, ',', n, '}'] = do -- TODO update pushglobal for data
    stack <- getStack
    globals <- getGlobals
    heap <- getHeap
    let a = Map.findWithDefault (-1) name globals
    a' <- case a of
        (-1) -> do
            let (heap', a') = hAlloc heap (NGlobal (digitToInt n) [Pack [t] (digitToInt n), Update 0, Unwind])
            putHeap heap'
            putGlobals $ Map.insert name a' globals -- (name, a') : globals
            return a'
        _ -> return a
    putStack $ a' : stack
pushglobal name = do
    stack <- getStack
    globals <- getGlobals
    let a = Map.findWithDefault (error $ "undeclared global " ++ name) name globals
    putStack $ a : stack
pushint :: Int -> GMonad ()
pushint num = do
    stack <- getStack
    globals <- getGlobals
    heap <- getHeap
    let a = Map.findWithDefault (-1) (show num) globals
    if a == -1
        -- node wasnt allocated yet, so now we do it
        then do
            let (heap', a) = hAlloc heap (NInt num)
            putStack (a : stack)
            putHeap heap'
            putGlobals $ Map.insert (show num) a globals -- ((show num, a) : globals)
        else do
            putStack (a : stack)
mkap :: GMonad ()
mkap = do
    stack <- getStack
    heap <- getHeap
    let (heap', a) = hAlloc heap (NApp (headDef (error "mkap no head found") stack ) (headDef (error "mkap no head . tail found") (tail stack)))
    putStack (a : drop 2 stack)
    putHeap heap'
push :: Int -> GMonad ()
push num = do
    stack <- getStack
    heap <- getHeap
    let a = stack !! num
    putStack $ a:stack

getArg :: Node -> Addr
getArg (NApp a1 a2) = a2
getArg _ = error "getArg called on non application!"

unwind :: GMonad ()
unwind = do
    heap <- getHeap
    stack <- getStack
    (i', s') <- headDef (error "unwind: no head of dump found") <$> getDump
    d' <- tail <$> getDump
    let a = headDef (error "unwind: no head of stack found") stack
    let node = (Map.!) heap a
    case node of
        NInt n -> putCode i' >> putStack (a : s') >> putDump d'
        NApp a1 a2 -> putStack (a1 : stack) >> putCode [Unwind]
        NGlobal n c -> if length stack - 1 >= n
                            then putCode c >> rearrange n
                            else putCode i' >> putStack (last stack : s') >> putDump d'
        NInd a1 -> putStack (a1 : tail stack) >> putCode [Unwind]
        NData n as -> putStack (headDef (error "unwind: no head of stack found") stack : s') >> putCode i' >> putDump d'

update :: Int -> GMonad ()
update n = do
    stack <- getStack
    heap <- getHeap
    -- TODO indirection just doesn't get created...
    let heap' = Map.insert (stack !! (n + 1)) (NInd (headDef (error "update: no head of stack found") stack)) heap
    let node = (Map.!) heap' (stack !! (n + 1))
    putHeap heap'
    putStack $ tail stack

pop :: Int -> GMonad ()
pop n = do
    stack <- getStack
    putStack (drop n stack)

doEval :: GMonad ()
doEval = do
    i <- getCode
    s <- getStack
    d <- getDump
    putDump $ (i, tail s) : d
    putStack [headDef (error "doEval: no head of stack found") s]
    putCode [Unwind]

slide :: Int -> GMonad ()
slide num = do
    stack <- getStack
    putStack (headDef (error "slide: no head of stack found") stack : drop (num + 1) stack)

alloc :: Int -> GMonad ()
alloc n = do
    nodes <- allocNodes n
    stack <- getStack
    putStack $ nodes ++ stack

allocNodes :: Int -> GMonad [Addr]
allocNodes 0 = return []
allocNodes n = do
    as <- allocNodes (n - 1)
    heap <- getHeap
    let (heap'', a) = hAlloc heap (NInd hNull)
    putHeap heap''
    return $ a : as

pack :: Identifier -> Int -> GMonad ()
pack t n = do
    stack <- getStack
    heap <- getHeap
    let args = take n stack
    let (heap', a) = hAlloc heap (NData t args)
    putHeap heap'
    putStack $ a : drop n stack

casejump :: [(Identifier, GCode)] -> GMonad ()
casejump ts = do
    stack <- getStack
    heap <- getHeap
    code <- getCode
    let node = (Map.!) heap (headDef (error "casejump: no head of stack found") stack)
    case node of
        (NData t ss) -> do
            let i' = Map.findWithDefault (error "casejump: tag of constructor not found in casejumps list!") t (Map.fromList ts)
            putCode $ i' ++ code
        o -> error $ "casejump: node on top of stack isnt constructor!! Instead: " ++ show o 

split :: Int -> GMonad ()
split n = do
    stack <- getStack
    heap <- getHeap
    let node = (Map.!) heap (headDef (error "split: no head of stack found") stack)
    case node of
        (NData t ss) -> putStack $ ss ++ tail stack
        _ -> error "split: node on top of stack isnt constructor!!"

boxBoolean :: Bool -> GMonad ()
boxBoolean b = do
    stack <- getStack
    heap <- getHeap
    let b' = if b then "true" else "false"
    let (h', a) = hAlloc heap (NData b' [])
    putHeap h'
    putStack $ a : stack

unboxBoolean :: Addr -> GMonad Bool
unboxBoolean a = do
    h <- getHeap
    return $ ub ((Map.!) h a)
    where
        ub (NData "true" []) = True
        ub (NData "false" []) = False
        ub _ = error "Unboxing a non-boolean!"

boxInteger :: Int -> GMonad()
boxInteger n = do
    s <- getStack
    h <- getHeap
    let (h', a) = hAlloc h (NInt n)
    putHeap h'
    putStack $ a : s

unboxInteger :: Addr -> GMonad Int
unboxInteger a = do
    h <- getHeap
    return $ ub ((Map.!) h a)
    where
        ub (NInt i) = i
        ub _ = error "Unboxing a non-integer!"

primitive1 :: (b -> GMonad ())
           -> (Addr -> GMonad a)
           -> (a -> b)
           -> GMonad ()
primitive1 box unbox op = do
    s <- getStack
    putStack $ tail s
    ub <- unbox $ headDef (error "primitive1: no head of stack found") s
    box $ op ub

primitive2 :: (b -> GMonad ())
           -> (Addr -> GMonad a)
           -> (a -> a -> b)
           -> GMonad ()
primitive2 box unbox op = do
    s <- getStack
    putStack $ drop 2 s
    ub0 <- unbox $ headDef (error "primitive2: no head of stack found") s
    ub1 <- unbox . headDef (error "primitive2: no head of tail stack found") $ tail s
    box $ op ub0 ub1

arithmetic1 :: (Int -> Int) -> GMonad ()
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GMonad ()
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> GMonad ()
comparison = primitive2 boxBoolean unboxInteger

--builtInDyadic :: Map Identifier Instruction
--builtInDyadic = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("==", Eq), ("~=", Ne), (">=", Ge), ("<=", Le), (">", Gt), ("<", Lt)]

-- runners
run :: [(Identifier, Int, Code)] -> String
run prog = showResults $ evalState eval (GState initialCode [] [] heap globals)
    where
        (heap, globals) = buildInitialHeap prog
        initialCode = [PushGlobal "main", Eval]

eval :: GMonad [GState]
eval = reverse <$> evalHelper []
    where
        evalHelper :: [GState] -> GMonad [GState]
        evalHelper states = do
            step
            final <- gFinal
            state <- get
            stack <- getStack
            heap <- getHeap
            if final
                then return $ state : states
                else evalHelper (state : states)

-- compiler

buildInitialHeap :: [(Identifier, Int, Code)] -> (Map Addr Node, Map Identifier Addr)
buildInitialHeap prog = makeResult $ mapAccumL allocateSc Map.empty compiled
    where
        makeResult (a,b) = (a, Map.fromList b)
        compiled = prog ++ compiledPrimitives

allocateSc :: Map Addr Node -> (Identifier, Int, [Instruction]) -> (Map Addr Node, (Identifier, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal nargs instns)

compiledPrimitives :: [(Identifier, Int, [Instruction])]
compiledPrimitives = [("add", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
                      ("sub", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
                      ("mul", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
                      ("div", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
                        ]

-- printing results
showResults :: [GState] -> String
showResults = show . last