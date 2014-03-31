module Text.Xml.Pickle where

import Control.Monad.Identity
import Control.Category
import Prelude hiding ((.))
import Text.XML.HXT.Core

toXML :: XmlPickler p => p -> String
toXML = showPickled []

fromXML :: XmlPickler a => String -> a
fromXML = runIdentity . fromXMLM

maybeFromXML :: XmlPickler a => String -> Maybe a
maybeFromXML = fromXMLM

eitherFromXML :: XmlPickler a => String -> Either String a
eitherFromXML text =
  case runLA (removeAllWhiteSpace . xread) text of
    []    -> Left "Failed to parse XML in fromXMLM."
    [x]   -> unpickleDoc' xpickle x
    (_:_) -> Left "Multiple parses in fromXMLM."

fromXMLM :: (Monad m, XmlPickler a) => String -> m a
fromXMLM text = case runLA (removeAllWhiteSpace . xread) text of
  []    -> fail "Failed to parse XML in fromXMLM."
  [x]   -> either fail return $ unpickleDoc' xpickle x
  (_:_) -> fail "Multiple parses in fromXMLM."
