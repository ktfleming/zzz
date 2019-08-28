{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Details where

import           Brick                          ( Widget
                                                , txt
                                                , vBox
                                                , (<+>)
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.Header            ( isHeaderEnabled )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Attr
import           UI.Forms.Headers               ( readOnlyHeaders )
import           UI.List                        ( ZZZList )
import           UI.Text                        ( explanationWithAttr
                                                , methodWidget
                                                )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

showRequestDefDetails
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = do
  s <- iget
  let ring = focusRing [RequestDetails, ResponseList, ResponseBodyDetails]
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c)) ring

refreshResponseList
  :: Monad m => IxStateT m (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag) ()
refreshResponseList = do
  s <- iget
  let RequestDefDetailsScreen c _ ring = s ^. screen
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c)) ring


requestDefDetailsWidget :: AppState a -> RequestDefContext -> Bool -> Widget Name
requestDefDetailsWidget s c@(RequestDefContext _ rid) focused =
  let
    r                = lookupRequestDef s c
    hasActiveRequest = Map.member rid (s ^. activeRequests)
    titleWidget =
      txt "Request: " <+> methodWidget (r ^. method) <+> txt (" " <> r ^. url . coerced)
    headersWidget = txt "Headers: " <+> readOnlyHeaders (S.filter isHeaderEnabled (r ^. headers))
    explanation   = case (hasActiveRequest, focused) of
      (True, _) ->
        [ explanationWithAttr importantExplanationAttr
                              "Currently sending request -- press x to cancel"
        ]
      (False, True ) -> [explanationWithAttr explanationAttr "Press ENTER to send this request"]
      (False, False) -> []
  in
    vBox $ explanation <> [titleWidget, headersWidget]
