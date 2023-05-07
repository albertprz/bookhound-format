module Bookhound.Format.SyntaxTrees.Json (JsonExpression(..)) where

import Bookhound.Utils.Foldable (stringify)
import Bookhound.Utils.Map      (showMap)

import Data.Char (toLower)
import Data.Map  (Map)



data JsonExpression
  = JsNumber Double
  | JsBool Bool
  | JsString String
  | JsArray [JsonExpression]
  | JsObject (Map String JsonExpression)
  | JsNull
  deriving (Eq, Ord)


instance Show JsonExpression where
  show = \case
    JsNull       -> "null"
    JsNumber n   -> show n
    JsBool bool  -> toLower <$> show bool
    JsString str -> show str
    JsArray arr  -> stringify ",\n" "[\n" "\n]" 2 $ show   <$> arr
    JsObject obj -> stringify ",\n" "{\n" "\n}" 2 $ showMap ": " id show obj
