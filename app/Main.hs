module Main where

import Control.Monad (when, unless)
import Data.List (genericTake)
import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser,
    argument,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    progDesc,
    short,
    showDefault,
    str,
    strOption,
    switch,
    value,
    (<**>),
  )
import Parser (parse)
import PrettyPrint (ppr)
import Data.Text
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Compiler
import Evaluator
import Results (showResults)

data CmdOption = CmdOption
  { sourceFile :: String,
    prettyPrint :: Bool,
    debug :: Bool
  }

cmdOption :: Parser CmdOption
cmdOption =
  CmdOption
    <$> argument
      str
      ( metavar "<source file>"
      )
    <*> switch
      ( short 'p'
          <> help "PrettyPrint the parsed program"
          <> showDefault
      )
    <*> switch
      ( short 'd'
          <> help "Debug the compiler, prints AST of program"
          <> showDefault
      )

main :: IO ()
main = Main.run =<< execParser opts
  where
    opts =
      info
        (cmdOption <**> helper)
        ( fullDesc
            <> progDesc "Compiles a given tulip source-code."
            <> header "The tulip compiler"
        )

run :: CmdOption -> IO ()
-- only lex and parse
run (CmdOption sourceFile ppr dbg) = do
  -- read sourceFile
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  -- do lex and parse
  let parsed = parse sourceFile sourceText
  let prog = case parsed of
        Left err -> error $ show err
        Right prog -> prog

  -- do pretty print (when specified)
  when ppr ( do 
      putStrLn "====================== Source ======================"
      putStrLn . unpack $ PrettyPrint.ppr prog
      putStrLn "===================================================="
    )
  -- print AST when in debug mode
  when dbg $ do
    putStrLn "======================= AST ======================="
    print prog
    putStrLn "==================================================="

  -- do compilation
  let compiled = compile prog
  
  -- run program
  let state = Evaluator.run compiled

  -- show result
  putStrLn $ showResults "terse" state
