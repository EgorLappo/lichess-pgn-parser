module Lib where

import Data.Map (fromList, (!))
import Data.Maybe (maybe)
import Control.Applicative ((<|>))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as Lazy
import qualified Data.Attoparsec.Text as TextP

import Replace.Attoparsec.Text (streamEdit)

-- https://stackoverflow.com/questions/27236539/what-about-data-attoparsec-bytestring-lazy-char8


data PGN = PGN 
  { site        :: Text
  , date        :: Text
  , whitePlayer :: Text
  , blackPlayer :: Text
  , whiteElo    :: Text
  , blackElo    :: Text
  , eco         :: Text
  , opening     :: Text
  , timeControl :: Text
  , result      :: Text
  , termination :: Text
  , moves       :: Text
  } deriving Show


parsePGN = maybe [] id . Lazy.maybeResult . (Lazy.parse game)

game = many1 $ do
  skipSpace
  tags <- fromList <$> many1 tag
  skipSpace
  moves <- moveLine
  return $ PGN (decodeLatin1 $ tags ! "Site") 
               (decodeLatin1 $ tags ! "Date") 
               (decodeLatin1 $ tags ! "White") 
               (decodeLatin1 $ tags ! "Black") 
               (decodeLatin1 $ tags ! "WhiteElo") 
               (decodeLatin1 $ tags ! "BlackElo") 
               (decodeLatin1 $ tags ! "ECO") 
               (decodeLatin1 $ tags ! "Opening") 
               (decodeLatin1 $ tags ! "TimeControl") 
               (decodeLatin1 $ tags ! "Result") 
               (decodeLatin1 $ tags ! "Termination") 
               (decodeLatin1 moves)

tag = do
  skipSpace
  char '['
  tagName <- takeTill ((==) ' ')
  skipSpace
  tagValue <- stringLiteral
  char ']'
  return (tagName, tagValue)

stringLiteral = do
  char '"'
  value <- takeTill ((==) '"')
  char '"'
  return value

moveLine = do
    skipSpace
    moves <- takeTill ((==) '\n')
    char '\n'
    return moves

cleanMoves :: PGN -> PGN
cleanMoves pgn = 
    pgn { moves = moves' }
  where
    -- parse the move string omitting anything between curly braces {}
    moves' = (T.unwords . init . T.words) $ streamEdit (extraInfo <|> moveNumber) (const "") (moves pgn)
    extraInfo = TextP.char '{' *> TextP.takeTill ((==) '}') <* TextP.char '}'
    moveNumber = do
        TextP.decimal 
        (TextP.many1 $ TextP.char '.')
        return ""

cutMoves :: Int -> PGN -> PGN
cutMoves n pgn = 
    pgn { moves = moves' }
  where
    moves' = T.intercalate "," $ Prelude.take n $ T.words $ moves pgn

processPGN :: PGN -> PGN 
processPGN pgn = 
    pgn { result = result' }
  where
    result' = case result pgn of
      "1-0"     -> "1"
      "0-1"     -> "-1"
      "1/2-1/2" -> "0"
      _         -> ""

colNames :: Text
colNames = "site,date,white,black,whiteElo,blackElo,eco,opening,timeControl,result,termination"

formatToCSV :: PGN -> Text
formatToCSV pgn = 
    site pgn <> "," <> date pgn <> "," <> whitePlayer pgn <> "," <> blackPlayer pgn <> "," <> whiteElo pgn <> "," <> blackElo pgn <> "," <> eco pgn <> "," <> opening pgn <> "," <> timeControl pgn <> "," <> result pgn <> "," <> termination pgn <> "," <> moves pgn