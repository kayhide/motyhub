{-# LANGUAGE TemplateHaskell #-}

module Mid.Crawler.Type where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import Control.Lens
import Control.Lens.TH
import Network.URI
import qualified Network.Wreq as Wreq
import Text.Xml.Lens
import GHC.Generics


type Response = Wreq.Response Lazy.ByteString

data Form = Form
  { _formAction :: URI
  , _formFields :: Map ByteString Text
  , _formDom :: Element
  } deriving (Show, Generic)

makeFields ''Form


data Link = Link
  { _linkHref :: URI
  , _linkAnchor :: Text
  , _linkDom :: Element
  } deriving (Show, Generic)

makeFields ''Link
