{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Text where

import Control.Lens
import Data.Aeson (Value, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Coerce
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Types.Classes.Fields
import Types.Models.Environment (Variable, VariableName (..), VariableValue (..))
import Types.Models.Header

-- If the provided text is valid JSON, return the prettified version.
tryPretty :: T.Text -> Maybe T.Text
tryPretty t = do
  decoded <- (decode . cs) t :: Maybe Value
  return $ (cs . encodePretty) decoded

-- Substitutes any variables like {{this}} inside the provided text
substitute :: Coercible T.Text a => [Variable] -> a -> a
substitute vars t =
  coerce $
    foldr
      ( \v curr ->
          T.replace ("{{" <> v ^. name . coerce <> "}}") (v ^. value . coerce) curr
      )
      (coerce t)
      vars

-- Substitutes variables in both the name and value of a Header
substituteHeader :: [Variable] -> Header -> Header
substituteHeader vars (Header n v) =
  Header (substitute vars n) (substitute vars v)
