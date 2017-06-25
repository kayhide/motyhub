module Mid.Crawler.DomSelector where

import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)


data DomFactor = DomName Text | DomId Text | DomClass Text
  deriving (Show)

newtype DomSelector = DomSelector [DomFactor]

instance IsString DomSelector where
  fromString = parseSelector . Text.pack

parseSelector :: Text -> DomSelector
parseSelector = DomSelector . fromMaybe [] . parseMaybe selector'
  where
    selector' =  many (name' <|> id' <|> class') :: Parser [DomFactor]
    chunk' = some $ noneOf ['#', '.']
    name' = DomName . Text.pack <$> chunk'
    id' = DomId . Text.pack <$> (char '#' >> chunk')
    class' = DomClass . Text.pack <$> (char '.' >> chunk')
