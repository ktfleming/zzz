{-# LANGUAGE OverloadedStrings #-}

module Utils.Text where

import           Control.Lens
import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.String.Conversions        ( cs )
import qualified Data.Text                     as T
import           Types.Classes.Fields
import           Types.Models.Environment       ( Variable
                                                , VariableName(..)
                                                , VariableValue(..)
                                                )

-- If the provided text is valid JSON, return the prettified version.
tryPretty :: T.Text -> Maybe T.Text
tryPretty t = do
  decoded <- (decode . cs) t :: Maybe Value
  return $ (cs . encodePretty) decoded

-- Substitutes any variables like {{this}} inside the provided text
substitute :: [Variable] -> T.Text -> T.Text
substitute vars t = foldr
  (\v curr -> T.replace ("{{" <> v ^. name . coerced <> "}}") (v ^. value . coerced) curr)
  t
  vars
