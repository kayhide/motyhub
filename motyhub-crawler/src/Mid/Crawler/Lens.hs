module Mid.Crawler.Lens where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad
import Control.Lens
import Text.Xml.Lens
import Network.URI

import Mid.Crawler.Type


forms
  :: (Applicative f, Indexable Int p, Contravariant f, AsHtmlDocument t)
  => p Form (f Form) -> t -> f t
forms = html . folding universe . named (only "form") . to toForm . folded


toForm :: Element -> Maybe Form
toForm element = Form <$> action' <*> return fields' <*> return element
  where
    action' =
      element ^. attr "action"
      >>= parseURIReference . Text.unpack

    fields' = Map.fromList $ do
      input' <- element ^.. folding universe . named (only "input")
      let key' = input' ^. attr "name"
          value' = input' ^. attr "value" . folded
      guard $ isJust key'
      return (Text.encodeUtf8 (fromJust key'), value')




links
  :: (Applicative f, Indexable Int p, Contravariant f, AsHtmlDocument t)
  => p Link (f Link) -> t -> f t
links = html . folding universe . named (only "a") . to toLink . folded

toLink :: Element -> Maybe Link
toLink element = Link <$> href' <*> return text' <*> return element
  where
    href' =
      element ^. attr "href"
      >>= parseURIReference . Text.unpack

    text' =
      element ^. folding universe . text . to (Text.unwords . Text.words)


domId
  :: (Applicative f, Contravariant f, HasDom s Element)
  => (Text -> f Text) -> s -> f s
domId = dom . attr "id" . folded

domClass
  :: (Applicative f, Contravariant f, HasDom s Element)
  => (Text -> f Text) -> s -> f s
domClass = dom . attr "class" . folded . to Text.words . folded
