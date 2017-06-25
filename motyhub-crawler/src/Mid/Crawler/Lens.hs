module Mid.Crawler.Lens where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Control.Monad
import Control.Lens
import Text.Xml.Lens
import Network.URI

import Mid.Crawler.Type
import Mid.Crawler.DomSelector


selected :: DomSelector -> Fold Element Element
selected (DomSelector factors) = folding universe . filtered' factors
  where
    filtered' [] = filtered (const True)
    filtered' (DomName x : xs) = named (only name') . filtered' xs
      where name' = fromString $ Text.unpack x
    filtered' (DomId x : xs) = attributed (ix "id" . only x) . filtered' xs
    filtered' (DomClass x : xs) = attributed (ix "class" . to Text.words . folded . only x) . filtered' xs


forms :: Fold Element Form
forms = selected "form" . _Form

_Form :: Fold Element Form
_Form = to toForm . folded

toForm :: Element -> Maybe Form
toForm element = Form <$> action' <*> return fields' <*> return element
  where
    action' =
      element ^. attr "action"
      >>= parseURIReference . Text.unpack . unescapeHtmlEntity

    fields' = Map.fromList $ do
      input' <- element ^.. inputs
      return (Text.encodeUtf8 $ input' ^. key, input' ^. value)


inputs :: Fold Element Input
inputs = selected "input" . _Input

_Input :: Fold Element Input
_Input = to toInput . folded

toInput :: Element -> Maybe Input
toInput element = Input <$> name' <*> return value' <*> return element
  where
    name' = element ^. attr "name"
    value' = element ^. attr "value" . folded


links ::Fold Element Link
links = selected "a" . _Link

_Link ::Fold Element Link
_Link = to toLink . folded

toLink :: Element -> Maybe Link
toLink element = Link <$> href' <*> return element
  where
    href' =
      element ^. attr "href"
      >>= parseURIReference . Text.unpack . unescapeHtmlEntity


domId :: (HasDom s Element) => Fold s Text
domId = dom . attr "id" . folded

domClass :: (HasDom s Element) => Fold s Text
domClass = dom . attr "class" . folded . to Text.words . folded

innerText :: (HasDom s Element) => Fold s Text
innerText = dom . folding universe . text . to (Text.unwords . Text.words)


unescapeHtmlEntity :: Text -> Text
unescapeHtmlEntity x = ((LazyText.encodeUtf8 . LazyText.fromStrict) x) ^. html . text
