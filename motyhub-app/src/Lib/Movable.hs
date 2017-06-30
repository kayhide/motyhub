module Lib.Movable where

import Data.Monoid
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Data.Time
import Data.Aeson
import Control.Arrow
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy (Parser)


parseItems :: LazyText.Text -> [Value]
parseItems doc = catMaybes $ parseItem . LazyText.pack <$> fromMaybe [] (parseMaybe chunks' doc)

parseItem :: LazyText.Text -> Maybe Value
parseItem doc = do
  (y:ys) <- parseMaybe parts' doc
  zs <- parseMaybe (many single') $ LazyText.pack y
  let zs' = catMaybes $ parseMaybe multi' . LazyText.pack <$> ys
  return $ object $ encode' <$> zs ++ zs'
  where
    encode' :: KeyValue kv => (String, String) -> kv
    encode' (k, v) = case k of
      "DATE" -> Text.pack k .= (toJSON . fromJust . parseMaybe date' . LazyText.pack) v
      otherwise -> Text.pack k .= toJSON v

chunks' :: Parser [String]
chunks' = endBy (many nonSeparator') separator'

parts' :: Parser [String]
parts' = endBy (many nonDelimiter') delimiter'

single' :: Parser (String, String)
single' = do
  k <- some $ noneOf [':']
  char ':' >> space
  v <- many (notFollowedBy eol >> anyChar)
  eol
  return (k, v)

multi' :: Parser (String, String)
multi' = do
  k <- some $ noneOf [':']
  char ':' >> eol
  v <- many anyChar
  return (k, v)


delimiter' = string "-----" >> some eol >> return () :: Parser ()
nonDelimiter' = notFollowedBy delimiter' >> anyChar :: Parser Char

separator' = string "--------" >> some eol >> return () :: Parser ()
nonSeparator' = notFollowedBy separator' >> anyChar :: Parser Char


date' :: Parser LocalTime
date' = do
  mon :: Int <- read <$> many digitChar
  char '/'
  day :: Int <- read <$> many digitChar
  char '/'
  year :: Integer <- read <$> many digitChar
  space
  hour :: Int <- read <$> many digitChar
  char ':'
  min :: Int <- read <$> many digitChar
  char ':'
  sec :: Int <- read <$> many digitChar
  space
  ampm <- (string "AM" >> return 0) <|> (string "PM" >> return 12)
  return $
    LocalTime
    (fromGregorian year mon day)
    (TimeOfDay (hour + ampm) min (fromIntegral sec))
