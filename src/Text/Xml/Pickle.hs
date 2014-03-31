-- | Some utility functions for using hxt picklers.
module Text.Xml.Pickle
  ( toXML
  , fromXML
  , maybeFromXML
  , eitherFromXML
  ) where

import Control.Monad.Identity
import Control.Category
import Prelude hiding ((.))
import Text.XML.HXT.Core

-- | Convert a value to an XML string.
toXML :: XmlPickler p => p -> String
toXML = showPickled []

-- | Parse a string containing xml to a value. On parse failure, will
-- call `error`.
fromXML :: XmlPickler a => String -> a
fromXML = runIdentity . fromXMLM

-- | Parse a string containing xml to a value, or `Nothing` if the
-- parse fails.
maybeFromXML :: XmlPickler a => String -> Maybe a
maybeFromXML = fromXMLM

-- | Parse a string containing xml to a value, or an error message on
-- failure.
eitherFromXML :: XmlPickler a => String -> Either String a
eitherFromXML text =
  case runLA (removeAllWhiteSpace . xread) text of
    []    -> Left "Failed to parse XML in eitherFromXML."
    [x]   -> unpickleDoc' xpickle x
    (_:_) -> Left "Multiple parses in eitherFromXML."

fromXMLM :: (Monad m, XmlPickler a) => String -> m a
fromXMLM text = case runLA (removeAllWhiteSpace . xread) text of
  []    -> fail "Failed to parse XML in fromXMLM."
  [x]   -> either fail return $ unpickleDoc' xpickle x
  (_:_) -> fail "Multiple parses in fromXMLM."
