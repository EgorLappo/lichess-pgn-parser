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
  , cut  :: !Int 
  }

optionsParser :: ParserInfo Options
optionsParser = 
    info (helper <*> options)
      ( fullDesc
     <> progDesc "convert a lichess pgn file into tabular format, streaming"
     <> header "lichess-pgn-parser. featuring: attoparsec" )
  where
    options = Options <$> 
      switch ( long "keep" <> short 'k' <> help "keep the move string as-is (e.g. with time, evaluation)" ) <*>
      option auto ( long "cut" <> short 'c' <> help "cut the move string after n moves, make individual columns" <> value 0 )

-- test out the options
main :: IO ()
main = do
  opts <- execParser optionsParser
  let ncut = cut opts
      k = keep opts

  f <- BSL.getContents
  let colNames' = if (cut opts) > 0 then colNames <> T.concat (map (\x -> ",move" <> (T.pack $ show x)) [1..(cut opts)]) else colNames <> ",moves"

  T.putStrLn $ colNames'

  let pgn = parsePGN f
  let pgn' = if k then pgn else map cleanMoves pgn
  let pgn'' = if (ncut > 0) then map (cutMoves ncut) pgn' else pgn'
  
  mapM_ T.putStrLn $ map (formatToCSV . processPGN) pgn''
  
  
  
    
