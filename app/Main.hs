module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
import Lib
import Options.Applicative

data Options = Options
  { keep :: !Bool,
    hd :: !Bool,
    sep :: !Text,
    cut :: !Int
  }

optionsParser :: ParserInfo Options
optionsParser =
  info
    (helper <*> options)
    ( fullDesc
        <> progDesc "convert a lichess pgn file into tabular format, streaming"
        <> header "lichess-pgn-parser. featuring: attoparsec"
    )
  where
    options =
      Options
        <$> switch (long "keep" <> short 'k' <> help "keep the move string as-is (e.g. with time, evaluation data)")
        <*> switch (long "header" <> short 'n' <> help "print the csv header line (column names)")
        <*> option auto (long "sep" <> short 's' <> help "separator character for output (default: ,)" <> value ",")
        <*> option auto (long "cut" <> short 'c' <> help "cut the move string after ARG moves, making individual columns  (default: 0)" <> value 0)

-- test out the options
main :: IO ()
main = do
  opts <- execParser optionsParser
  let ncut = cut opts
      k = keep opts
      h = hd opts
      s = sep opts

  when ((ncut > 0) && k) $ error "cannot use both --keep and --cut"

  if k
    then runKeep h s
    else
      if ncut > 0
        then runCut ncut h s
        else runDef h s
