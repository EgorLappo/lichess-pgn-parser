module Lib (runKeep, runCut, runDef) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Lazy qualified as Lazy
import Data.Attoparsec.Text qualified as TextP
import Data.ByteString.Lazy qualified as BSL
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1)
import Data.Text.IO qualified as T
import Replace.Attoparsec.Text (streamEdit)

-- https://stackoverflow.com/questions/27236539/what-about-data-attoparsec-bytestring-lazy-char8

data PGN = PGN
  { site :: Text,
    date :: Text,
    whitePlayer :: Text,
    blackPlayer :: Text,
    whiteElo :: Text,
    blackElo :: Text,
    whiteTitle :: Text,
    blackTitle :: Text,
    eco :: Text,
    timeControl :: Text,
    result :: Text,
    termination :: Text,
    variant :: Text,
    moves :: Text
  }
  deriving (Show)

-- generic function to run the parser from stdin
run ::
  (PGN -> PGN) -> -- optional function to process the PGN before formatting to CSV
  Text -> -- header line with column names
  Bool -> -- whether to print the header line
  Text -> -- separator character as Text
  IO ()
run f colNames' h sep = do
  when h $ T.putStrLn colNames'
  flines <- BSL.getContents
  goRec (Lazy.parse game flines)
  where
    goRec :: Lazy.Result PGN -> IO ()
    goRec (Lazy.Fail _ _ e) = error $ "parser failed: " <> e
    goRec (Lazy.Done bs pgn) =
      if BSL.null bs
        then T.putStrLn . formatToCSV sep . processPGN . f $ pgn
        else do
          T.putStrLn . formatToCSV sep . processPGN . f $ pgn
          goRec (Lazy.parse game bs)

runKeep h sep = run id colNames' h sep
  where
    colNames' = T.intercalate sep colNames <> sep <> "moves"

runDef h sep = run (cleanMoves 0) colNames' h sep
  where
    colNames' = T.intercalate sep colNames <> sep <> "moves"

runCut ncut h sep = run (cleanMoves ncut) colNames' h sep
  where
    colNames' = T.intercalate sep colNames <> sep <> T.intercalate sep (map (\i -> "move" <> T.pack (show i)) [1 .. ncut])

-- i borrow a lot from https://hackage.haskell.org/package/chesshs here
-- in terms of parsing the tags and the whole PGN type

game = do
  tags <- Map.fromList <$> many1 tag
  skipSpace
  moves' <- moveLine
  skipSpace
  return $
    PGN
      (decodeLatin1 $ tags ! "Site")
      (decodeLatin1 $ tags ! "UTCDate")
      (decodeLatin1 $ tags ! "White")
      (decodeLatin1 $ tags ! "Black")
      (decodeLatin1 $ tags ! "WhiteElo")
      (decodeLatin1 $ tags ! "BlackElo")
      (optional tags "WhiteTitle")
      (optional tags "BlackTitle")
      (decodeLatin1 $ tags ! "ECO")
      (decodeLatin1 $ tags ! "TimeControl")
      (decodeLatin1 $ tags ! "Result")
      (decodeLatin1 $ tags ! "Termination")
      (optional tags "Variant")
      (decodeLatin1 moves')
  where
    optional t v = maybe "" decodeLatin1 (Map.lookup v t)

tag = do
  skipSpace
  char '['
  tagName <- takeTill (' ' ==)
  skipSpace
  tagValue <- stringLiteral
  char ']'
  return (tagName, tagValue)

stringLiteral = do
  char '"'
  value <- takeTill ('"' ==)
  char '"'
  return value

moveLine = do
  skipSpace
  moves' <- takeTill ('\n' ==)
  char '\n'
  return moves'

cleanMoves :: Int -> PGN -> PGN
cleanMoves 0 pgn =
  pgn {moves = moves'}
  where
    -- parse the move string omitting anything between curly braces {}
    moves' = T.unwords . map stripMarks . init . T.words $ streamEdit (extraInfo <|> moveNumber) (const "") (moves pgn)
    extraInfo = TextP.char '{' *> TextP.takeTill ('}' ==) <* TextP.char '}'
    moveNumber = do
      (_ :: Int) <- TextP.decimal
      TextP.many1 $ TextP.char '.'
      return ""
    stripMarks = T.filter (\c -> c /= '?' && c /= '!')
cleanMoves n pgn =
  pgn {moves = moves''}
  where
    -- parse the move string omitting anything between curly braces {}
    mlist = map stripMarks . init . T.words $ streamEdit (extraInfo <|> moveNumber) (const "") (moves pgn)

    extraInfo = TextP.char '{' *> TextP.takeTill ('}' ==) <* TextP.char '}'
    moveNumber = do
      (_ :: Int) <- TextP.decimal
      TextP.many1 $ TextP.char '.'
      return ""
    stripMarks = T.filter (\c -> c /= '?' && c /= '!')

    l = length mlist
    moves' = T.intercalate "," $ Prelude.take n mlist
    moves''
      | l >= n = moves'
      | l == 0 = T.concat $ replicate (n - l - 1) "," -- case no moves
      | otherwise = moves' <> T.concat (replicate (n - l) ",") -- case not enough moves

-- cutMoves :: Int -> PGN -> PGN
-- cutMoves n pgn =
--   pgn {moves = moves''}
--   where
--     mlist = T.words $ moves pgn
--     l = length mlist
--     moves' = T.intercalate "," $ Prelude.take n mlist
--     moves''
--       | l >= n = moves'
--       | l == 0 = T.concat $ replicate (n - l - 1) ","
--       | otherwise = moves' <> T.concat (replicate (n - l) ",")

processPGN :: PGN -> PGN
processPGN pgn =
  pgn {result = result', whiteElo = whiteElo', blackElo = blackElo'}
  where
    result' = case result pgn of
      "1-0" -> "1"
      "0-1" -> "-1"
      "1/2-1/2" -> "0"
      _ -> ""
    whiteElo' = case whiteElo pgn of
      "?" -> "0"
      _ -> whiteElo pgn
    blackElo' = case blackElo pgn of
      "?" -> "0"
      _ -> blackElo pgn

colNames :: [Text]
colNames = ["site", "date", "white", "black", "whiteElo", "blackElo", "eco", "timeControl", "result", "termination", "variant"]

formatToCSV :: Text -> PGN -> Text
formatToCSV sep pgn =
  site pgn <> sep <> date pgn <> sep <> whitePlayer pgn <> sep <> blackPlayer pgn <> sep <> whiteElo pgn <> sep <> blackElo pgn <> sep <> eco pgn <> sep <> timeControl pgn <> sep <> result pgn <> sep <> termination pgn <> sep <> variant pgn <> sep <> moves pgn
