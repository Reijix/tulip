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

data CmdOption = CmdOption
  { sourceFile :: String,
    prettyPrint :: Bool
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
run (CmdOption sourceFile ppr) = do
  -- read sourceFile
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  -- do lex and parse
  let parsed = parse sourceFile sourceText
  let prog = case parsed of
        Left err -> error $ show err
        Right prog -> prog

  when ppr (putStrLn . unpack $ PrettyPrint.ppr prog)
  unless ppr $ print prog

  -- do compilation
  let compiled = compile prog
  
  --print compiled
  print $ Evaluator.run compiled
  return ()
