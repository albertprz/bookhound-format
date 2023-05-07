module Bookhound.Format.Parsers.Json (json, nil, number, bool, string, array, object) where

import Bookhound.Parser              (Parser, withError)
import Bookhound.ParserCombinators   (IsMatch (..), maybeWithin, (<|>), (|*))
import Bookhound.Parsers.Char        (colon, doubleQuote)
import Bookhound.Parsers.Collections (listOf, mapOf)
import Bookhound.Parsers.Number      (double)
import Bookhound.Parsers.String      (spacing, withinDoubleQuotes)

import Bookhound.Format.SyntaxTrees.Json (JsonExpression (..))


json :: Parser JsonExpression
json = maybeWithin spacing jsValue
  where
    jsValue = element <|> container


nil :: Parser JsonExpression
nil = withError "Json Null" $
  JsNull <$ is "null"

number :: Parser JsonExpression
number = withError "Json Number" $
  JsNumber <$> double


bool :: Parser JsonExpression
bool = withError "Json Bool" $
  JsBool <$> (True  <$ is "true" <|>
              False <$ is "false")


string :: Parser JsonExpression
string = withError "Json String" $
  JsString <$> text


array :: Parser JsonExpression
array = withError "Json Array" $
  JsArray <$> listOf json


object :: Parser JsonExpression
object = withError "Json Object" $
  JsObject <$> mapOf colon text json


element :: Parser JsonExpression
element = number <|> bool <|> nil <|> string

container :: Parser JsonExpression
container = array <|> object


text :: Parser String
text = withinDoubleQuotes (inverse doubleQuote |*)
