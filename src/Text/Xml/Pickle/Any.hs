{-# LANGUAGE TupleSections #-}
module Text.Xml.Pickle.Any (xpAnyElem, xpAnyAttr) where

import Control.Monad.State (gets, modify)
import Text.XML.HXT.Arrow.Pickle (PU (PU, appPickle, appUnPickle, theSchema), xpAttr, xpCheckEmpty,
                                  xpCheckEmptyContents, xpElem)
import Text.XML.HXT.Arrow.Pickle.Schema (Schema (Any))
import Text.XML.HXT.Arrow.Pickle.Xml (Unpickler, attributes, getCont, liftMaybe, liftUnpickleVal,
                                      liftUnpickleVal, nesting, throwMsg, unpickleElem')
import Text.XML.HXT.Core (XmlTree, localPart)
import qualified Text.XML.HXT.DOM.XmlNode as XN

xpAnyElem :: PU b -> PU (String, b)
xpAnyElem pb = PU
  { appPickle = \(x, y) -> appPickle (xpElem x pb) y
  , appUnPickle = do
      t <- getCont
      n <- liftMaybe "xpElem: XML element expected" $ XN.getElemName t
      l <- gets nesting
      fmap (localPart n,) <$> liftUnpickleVal $ unpickleElem' (xpCheckEmpty pb) (l + 1) t
  , theSchema = Any
  }

xpAnyAttr :: PU a -> PU (String, a)
xpAnyAttr pa = PU
  { appPickle = \(n, x) -> appPickle (xpAttr n pa) x
  , appUnPickle = do
      a <- getAnyAtt
      l <- gets nesting
      nm <- maybe (throwMsg "impossible: not an attr in xpAttrWith")
                  (return . localPart)
                  (XN.getAttrName a)
      fmap (nm,) $ liftUnpickleVal $ unpickleElem' (xpCheckEmptyContents pa) l a
  , theSchema = Any
  }

getAnyAtt :: Unpickler XmlTree
getAnyAtt = do
  as <- gets attributes
  case as of
    [] -> throwMsg "no attribute value found"
    (a:as') -> do
      modify (\s -> s { attributes = as' })
      return $ nonEmptyVal a
  where
    nonEmptyVal a'
        | null (XN.getChildren a') = XN.setChildren [et] a'
        | otherwise                = a'
        where
          et = XN.mkText ""
