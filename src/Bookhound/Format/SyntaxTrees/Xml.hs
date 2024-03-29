module Bookhound.Format.SyntaxTrees.Xml (XmlExpression(..), literalExpression, flatten, findAll, find) where

import Bookhound.Utils.Foldable (stringify)

import Data.Maybe (listToMaybe)

import           Data.Map (Map, elems, keys)
import qualified Data.Map as Map


data XmlExpression
  = XmlExpression
      { tagName     :: String
      , fields      :: Map String String
      , expressions :: [XmlExpression]
      }
  deriving (Eq, Ord)


instance Show XmlExpression where

  show XmlExpression { tagName = tag, .. }
    | tag == "literal" = head . elems $ fields
    | otherwise        = "<" <> tag <> flds <> innerExprs
    where
      innerExprs = if | null expressions -> "/>"
                      | otherwise        -> ">"  <> ending

      (sep, n) = if | ((tagName) . head) expressions == "literal" -> ("", 0)
                    | otherwise                                   -> ("\n", 2)

      ending = stringify sep sep sep n (show <$> expressions) <> "</" <> tag <> ">"

      flds | null fields = ""
           | otherwise = " " <> fieldsString
        where
          fieldsString = stringify " " "" "" 0 $ showFn <$> tuples
          showFn (x, y) = x <> "=" <> show y
          tuples = zip (keys fields) (elems fields)



literalExpression :: String -> XmlExpression
literalExpression val = XmlExpression { tagName = "literal",
                                        fields = Map.fromList [("value", val)],
                                        expressions = [] }


flatten :: XmlExpression -> [XmlExpression]
flatten expr = expr : expressions expr >>= flatten


findAll :: (XmlExpression -> Bool) -> XmlExpression -> [XmlExpression]
findAll f = filter f . flatten


find :: (XmlExpression -> Bool) -> XmlExpression -> Maybe XmlExpression
find f = listToMaybe . findAll f
