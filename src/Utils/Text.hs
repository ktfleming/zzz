module Utils.Text where

import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.String.Conversions        ( cs )
import qualified Data.Text                     as T

-- If the provided text is valid JSON, return the prettified version.
tryPretty :: T.Text -> Maybe T.Text
tryPretty t = do
  decoded <- (decode . cs) t :: Maybe Value
  return $ (cs . encodePretty) decoded
