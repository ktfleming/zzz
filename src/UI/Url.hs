{-# LANGUAGE OverloadedStrings #-}

module UI.Url
  ( colorizedUrl,
  )
where

import Brick
import Control.Lens
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Parsing.TemplatedTextParser
  ( TemplatedText (..),
    TemplatedTextPart (..),
    parseTemplatedText,
  )
import Text.Megaparsec (runParser)
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Models.Environment
  ( Variable,
    VariableName (..),
    VariableValue (..),
  )
import Types.Models.Url (Url (..))
import UI.Attr (templatedVariableAttr)

colorizedUrl :: Seq Variable -> Url -> Widget Name
colorizedUrl vars (Url t) = fromMaybe (txt t) $ do
  parsed <- rightToMaybe $ runParser parseTemplatedText "Templated URL" t
  pure $ addColors vars parsed

addColors :: Seq Variable -> TemplatedText -> Widget Name
addColors vars (TemplatedText urlParts) =
  foldr
    (\part curr -> partToWidget part <+> curr)
    emptyWidget
    urlParts
  where
    partToWidget :: TemplatedTextPart -> Widget Name
    partToWidget (TemplateVariable t) = case find (\v -> v ^. name . coerced == t) vars of
      Just v -> withAttr templatedVariableAttr (txt (v ^. value . coerced))
      Nothing -> txt $ "{{" <> t <> "}}" -- have to restore the {{ }} that was stripped by the parser
    partToWidget (TextPart t) = txt t
