module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as BSL

import Options.Applicative

import Lib

data Options = Options
  { 
    keep :: !Bool
  }

optionsParser :: ParserInfo Options
optionsParser = 
    info (helper <*> options)
      ( fullDesc
     <> progDesc "convert a lichess pgn file into tabular format, streaming"
     <> header "lichess-pgn-parser. featuring: attoparsec" )
  where
    options = Options <$> 
      switch ( long "keep" <> short 'k' <> help "keep the move string as-is (e.g. with time, evaluation)" )

-- test out the options
main :: IO ()
main = do
  opts <- execParser optionsParser

  f <- BSL.getContents
  let pgn = parsePGN f
      pgn' = if (keep opts) then pgn else map cleanMoves pgn

  T.putStrLn $ colNames
  mapM_ T.putStrLn $ map (formatToCSV . processPGN) pgn'
  
  
  
    
