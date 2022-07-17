{-# LANGUAGE UndecidableInstances #-}

module Bookhound.Format.Operations.ToYaml (ToYaml(..)) where

import Bookhound.Format.Operations.ToJson (ToJson (..))
import Bookhound.Format.SyntaxTrees.Json  (JsExpression (..))
import Bookhound.Format.SyntaxTrees.Yaml  (CollectionType (..), YamlExpression (..))

import Bookhound.Parser            (runParser)
import Bookhound.Parsers.Number    (intLike)

import Data.Text                   (pack)



class ToYaml a where
  toYaml :: a -> YamlExpression

instance {-# OVERLAPPABLE #-} ToJson a => ToYaml a where
  toYaml = toYaml . toJson

instance ToYaml YamlExpression where
  toYaml = id

instance ToYaml JsExpression where

  toYaml = \case
    JsNull       -> YamlNull
    JsNumber n   -> either (const (YamlFloat n)) YamlInteger $
      runParser intLike $ pack $ show n
    JsBool bool  -> YamlBool bool
    JsString str -> YamlString str
    JsArray arr  -> YamlList Standard $ toYaml <$> arr
    JsObject obj -> YamlMap  Standard $ toYaml <$> obj
