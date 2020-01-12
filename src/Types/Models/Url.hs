{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Models.Url where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req (parseUrl)
import Safe (headMay)

newtype Url = Url Text deriving (Show, FromJSON, ToJSON, Eq)

-- TODO: with the introduction of variables that can be used in URLs,
-- the following validation functions aren't used anymore. Could take them out.

-- To be used with Brick's `editField`. Given a list of Texts, validate it and possibly return
-- a Url newtype. Note that the edit field we provide will only have one line, so we only need
-- to look at the head of this list.
validateUrl :: [Text] -> Maybe Url
validateUrl ts = headMay ts >>= validateUrl'

-- Helper function for `validateUrl`, this just looks at once Text value since, as written above,
-- we only care about a single line of Text. To check if it's a valid URL, we call req's parseUrl
-- and then throw away the output; we only care if the parse returns Just or not.
validateUrl' :: Text -> Maybe Url
validateUrl' t = Url t <$ parseUrl (encodeUtf8 t)
