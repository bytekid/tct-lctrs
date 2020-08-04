-- | This module provides functions for generating XML content.
module Tct.Core.Common.Xml
  ( Xml (..)
  , XmlContent
  , XmlAttribute
  , XmlDocument
  , elt
  , empty
  , unsupported
  , int
  , text
  , att
  , setAtts
  -- * output
  , toDocument
  , putXml
  , putXmlHandle
  -- * search and manipulation
  , search
  , find
  , rootTag
  , child
  , children
  , addChildren
  ) where



import qualified Data.ByteString.Lazy  as BS
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as Txt
import qualified Data.Text.IO          as Txt (hPutStr, putStr)
import           GHC.IO.Handle
import qualified Text.XML.Expat.Format as Xml (formatNode)
import qualified Text.XML.Expat.Proc   as Xml
import qualified Text.XML.Expat.Tree   as Xml


type XmlContent   = Xml.UNode Txt.Text
type XmlAttribute = (Txt.Text, Txt.Text)
type XmlDocument  = (Txt.Text, XmlContent)

class Xml a where
  toXml  :: a -> XmlContent
  toCeTA :: a -> XmlContent
  toCeTA = const unsupported

instance Xml () where
  toXml _  = empty
  toCeTA _ = empty

elt :: String -> [XmlContent] -> XmlContent
elt name = Xml.Element (Txt.pack name) []

empty :: XmlContent
empty = text ""

setAtts :: XmlContent -> [XmlAttribute] -> XmlContent
setAtts e atts = e{ Xml.eAttributes = atts }

att :: String -> String -> XmlAttribute
att n s = (Txt.pack n, Txt.pack s)

unsupported :: XmlContent
unsupported = elt "unsupported" []

int :: (Integral i) => i -> XmlContent
int i = Xml.Text . Txt.pack . show $ toInteger i

text :: String -> XmlContent
text = Xml.Text . Txt.pack


toDocument :: Maybe String -> XmlContent -> XmlDocument
toDocument Nothing c  = (mempty, c)
toDocument (Just s) c = (Txt.pack $ header ++ s ++ "\n", c)
  where header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

putXml :: XmlDocument -> IO ()
putXml (header, content)= Txt.putStr header >> BS.putStr (Xml.formatNode content)

putXmlHandle :: XmlDocument -> Handle -> IO ()
putXmlHandle (header, content) h = Txt.hPutStr h header >> BS.hPutStr h (Xml.formatNode content)


search :: String -> XmlContent -> Maybe XmlContent
search s = Xml.findElement (Txt.pack s)

find :: String -> XmlContent -> XmlContent
find s c = err `fromMaybe` search s c
  where err = error $ "Tct.Core.Common.Xml.find: element not found " ++ s ++ " ."

rootTag :: XmlContent -> String
rootTag (Xml.Element t _ _) = Txt.unpack t
rootTag _                   = ""

child :: XmlContent -> XmlContent
child (Xml.Element _ _ [e]) = e
child _                     = error "Tct.Core.Common.Xml.children: not a single child."

children :: XmlContent -> [XmlContent]
children (Xml.Element _ _ es) = es
children (Xml.Text _)         = []

-- | Adds elements below the root element.
addChildren :: XmlContent -> [XmlContent] -> XmlContent
addChildren (Xml.Element n as es1) es2 = Xml.Element n as (es1 ++ es2)
addChildren e _                        = e


