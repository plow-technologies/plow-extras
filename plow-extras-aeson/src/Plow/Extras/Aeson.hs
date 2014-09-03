module Plow.Extras.Aeson where

-- Encoding
import Data.Aeson.Encode 
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson
import Data.Text

typeToText :: ToJSON a => a -> Text
typeToText = toStrict . toLazyText . encodeToTextBuilder . toJSON

valueToText :: Value -> Text
valueToText = toStrict . toLazyText . encodeToTextBuilder 


