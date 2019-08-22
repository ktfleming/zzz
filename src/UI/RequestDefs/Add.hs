{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Add where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , formState
                                                , newForm
                                                , setFormConcat
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import           Data.UUID.V4                   ( nextRandom )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasName
import           Types.Methods
import           Types.Models.Id                ( RequestDefId(..) )
import           Types.Models.Project           ( ProjectContext(..)
                                                , requestDefs
                                                )
import           Types.Models.RequestDef
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Form                        ( ZZZForm
                                                , spacedConcat
                                                )
import           UI.Forms.Headers               ( makeHeadersForm )

finishAddingRequestDef
  :: MonadIO m => IxStateT m (AppState 'RequestDefAddTag) (AppState 'RequestDefAddTag) ()
finishAddingRequestDef = do
  s <- iget
  let RequestDefAddScreen (ProjectContext pid) form = s ^. screen
  rid <- liftIO $ RequestDefId <$> nextRandom
  let req = RequestDef { requestDefName    = formState form ^. name
                       , requestDefUrl     = formState form ^. url
                       , requestDefMethod  = formState form ^. method
                       , requestDefHeaders = formState form ^. headers
                       }
  imodify $ projects . at pid . _Just . requestDefs . at rid ?~ req

makeAddRequestDefForm :: ZZZForm RequestDefFormState
makeAddRequestDefForm = setFormConcat spacedConcat $ newForm
  [ (txt "Name:     " <+>) @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
  , (txt "URL:      " <+>) @@= editTextField (url . coerced) RequestDefFormUrlField (Just 1)
  , (txt "Headers:  " <+>) @@= makeHeadersForm
  ]
  RequestDefFormState { requestDefFormStateName    = RequestDefName "New Request Definition"
                      , requestDefFormStateUrl     = Url "http://example.com"
                      , requestDefFormStateMethod  = Get
                      , requestDefFormStateHeaders = S.empty
                      }

showAddRequestDefScreen
  :: Monad m => ProjectContext -> IxStateT m (AppState a) (AppState 'RequestDefAddTag) ()
showAddRequestDefScreen c = imodify $ screen .~ RequestDefAddScreen c makeAddRequestDefForm
