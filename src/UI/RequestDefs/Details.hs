{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Details where

import           Brick                          ( Widget
                                                , txt
                                                , (<+>)
                                                , (<=>)
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Displayable      ( display )
import           Types.Classes.Fields
import           Types.Models.Header            ( isHeaderEnabled )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Forms.Headers               ( readOnlyHeaders )
import           UI.List                        ( ZZZList )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

showRequestDefDetails
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = do
  s <- iget
  let rs   = lookupResponses s c
      ring = focusRing [ResponseList, ResponseBodyDetails]
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList rs) ring

requestDefDetailsWidget :: AppState a -> RequestDefContext -> Widget Name
requestDefDetailsWidget s c =
  let
    r             = lookupRequestDef s c
    titleWidget   = txt $ "Request: " <> display (r ^. method) <> " " <> (r ^. url . coerced)
    headersWidget = txt "Headers: " <+> readOnlyHeaders (S.filter isHeaderEnabled (r ^. headers))
  in
    titleWidget <=> headersWidget
