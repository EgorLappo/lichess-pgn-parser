module Lib where

import           Control.Applicative              ((<|>))
import           Data.Map                         (fromList, (!))
import           Data.Maybe                       (maybe)

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import qualified Data.Text.IO                     as T

import qualified Data.ByteString.Lazy             as BSL

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy  as Lazy
import qualified Data.Attoparsec.Text             as TextP

import           Replace.Attoparsec.Text          (streamEdit)

-- https://stackoverflow.com/questions/27236539/what-about-data-attoparsec-bytestring-lazy-char8


data PGN = PGN
  { site        :: Text
  , date        :: Text
  , whitePlayer :: Text
  , blackPlayer :: Text
  , whiteElo    :: Text
  , blackElo    :: Text
  , eco         :: Text
  , timeControl :: Text
  , result      :: Text
  , termination :: Text
  , moves       :: Text
  } deriving Show

-- generic function to run the parser from stdin
run
  :: (PGN -> PGN) -- optional function to process the PGN before formatting to CSV
  -> Text -- header line with column names
  -> Bool -- whether to print the header line
  -> Text -- separator character as Text
  -> IO ()
run f colNames h sep = do
    if h then T.putStrLn colNames else return ()
    f <- BSL.getContents
    goRec (Lazy.parse game f)
  where
    goRec :: Lazy.Result PGN -> IO ()
    goRec (Lazy.Fail _ _ e) = error $ "parser failed: " <> e
    goRec (Lazy.Done bs pgn) = if (BSL.null bs)
      then (return ())
      else do
        T.putStrLn . formatToCSV sep . processPGN . f $ pgn
        goRec (Lazy.parse game bs)

runKeep h sep = run id colNames' h sep
  where colNames' = (T.intercalate sep colNames) <> sep <> "moves"

runDef h sep = run (cleanMoves 0) colNames' h sep
  where colNames' = (T.intercalate sep colNames) <> sep <> "moves"

runCut ncut h sep = run (cleanMoves ncut) colNames' h sep
  where colNames' = T.intercalate sep colNames <> sep <> T.intercalate sep (map (\i -> "move" <> T.pack (show i)) [1..ncut])

-- i borrow a lot from https://hackage.haskell.org/package/chesshs here
-- in terms of parsing the tags and the whole PGN type

game = do
  tags <- fromList <$> many1 tag
  skipSpace
  moves <- moveLine
  skipSpace
  return $ PGN (decodeLatin1 $ tags ! "Site")
               (decodeLatin1 $ tags ! "UTCDate")
               (decodeLatin1 $ tags ! "White")
               (decodeLatin1 $ tags ! "Black")
               (decodeLatin1 $ tags ! "WhiteElo")
               (decodeLatin1 $ tags ! "BlackElo")
               (decodeLatin1 $ tags ! "ECO")
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

cleanMoves :: Int -> PGN -> PGN
cleanMoves 0 pgn =
    pgn { moves = moves' }
  where
    -- parse the move string omitting anything between curly braces {}
    moves' = (T.unwords . map stripMarks . init . T.words) $ streamEdit (extraInfo <|> moveNumber) (const "") (moves pgn)
    extraInfo = TextP.char '{' *> TextP.takeTill ((==) '}') <* TextP.char '}'
    moveNumber = do
        TextP.decimal
        (TextP.many1 $ TextP.char '.')
        return ""
    stripMarks = T.filter (\c -> c /= '?' && c /= '!')
cleanMoves n pgn =
    pgn { moves = moves'' }
  where
    -- parse the move string omitting anything between curly braces {}
    mlist = (map stripMarks . init . T.words) $ streamEdit (extraInfo <|> moveNumber) (const "") (moves pgn)

    extraInfo = TextP.char '{' *> TextP.takeTill ((==) '}') <* TextP.char '}'
    moveNumber = do
        TextP.decimal
        (TextP.many1 $ TextP.char '.')
        return ""
    stripMarks = T.filter (\c -> c /= '?' && c /= '!')

    l = length mlist
    moves' = T.intercalate "," $ Prelude.take n $ mlist
    moves'' = if l >= n then moves' -- case enough moves
              else if l == 0 then (T.concat $ Prelude.take (n-l-1) $ repeat ",") -- case no moves
              else moves' <> (T.concat $ Prelude.take (n - l) $ repeat ",") -- case not enough moves

cutMoves :: Int -> PGN -> PGN
cutMoves n pgn =
    pgn { moves = moves'' }
  where
    mlist = T.words $ moves pgn
    l = length mlist
    moves' = T.intercalate "," $ Prelude.take n $ mlist
    moves'' = if l >= n then moves' else if l == 0 then (T.concat $ Prelude.take (n-l-1) $ repeat ",") else moves' <> (T.concat $ Prelude.take (n - l) $ repeat ",")


processPGN :: PGN -> PGN
processPGN pgn =
    pgn { result = result', whiteElo = whiteElo', blackElo = blackElo' }
  where
    result' = case result pgn of
      "1-0"     -> "1"
      "0-1"     -> "-1"
      "1/2-1/2" -> "0"
      _         -> ""
    whiteElo' = case whiteElo pgn of
      "?" -> "0"
      _   -> whiteElo pgn
    blackElo' = case blackElo pgn of
      "?" -> "0"
      _   -> blackElo pgn

colNames :: [Text]
colNames = ["site","date","white","black","whiteElo","blackElo","eco","timeControl","result","termination"]

formatToCSV :: Text -> PGN -> Text
formatToCSV sep pgn =
    site pgn <> sep <> date pgn <> sep <> whitePlayer pgn <> sep <> blackPlayer pgn <> sep <> whiteElo pgn <> sep <> blackElo pgn <> sep <> eco pgn <> sep <> timeControl pgn <> sep <> result pgn <> sep <> termination pgn <> sep <> moves pgn
