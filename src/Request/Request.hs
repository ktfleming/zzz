{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.Request
  ( sendRequest
  )
where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

import           Types.Models.RequestDef

import           Brick                          ( EventM )
import           Control.Error
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Morph            ( hoist )
import           Control.Monad.Trans.Class      ( lift )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time                      ( getCurrentTime )
import           Data.Time.Clock                ( UTCTime )
import           Messages.Messages              ( logMessage )
import qualified Network.HTTP.Req              as Req
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Classes.HasId            ( model )
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )


type EitherReq
  = Either (Req.Url 'Req.Http, Req.Option 'Req.Http) (Req.Url 'Req.Https, Req.Option 'Req.Https)

-- This is the function called from the event handler; it uses the same monad stack that all
-- the event handlers use/require. But our main function that sends the request has an extra
-- ExceptT layer that the event handlers don't have, so we have to handle the error (if present)
-- by logging it, and then removing the ExceptT layer.
sendRequest
  :: RequestDefContext
  -> IxStateT
       (EventM Name)
       (AppState 'RequestDefDetailsTag)
       (AppState 'RequestDefDetailsTag)
       ()
sendRequest c = do
  result <- runExceptT $ sendRequest' c
  case result of
    Left  msg -> logMessage (T.pack msg)
    Right ()  -> return ()

-- Seems like the sendRequest' function needs some help with type annotations, so this alias will make it
-- not so verbose.
type Step a
  = ExceptT
      String
      (IxStateT (EventM Name) (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag))
      a

-- Since the HTTP request can throw an exception, this function adds an ExceptT to the top of the monad stack to
-- deal with that case.
sendRequest' :: RequestDefContext -> Step ()
sendRequest' c@(RequestDefContext _ rid) = do
  s <- lift iget :: Step (AppState 'RequestDefDetailsTag)
  let r :: RequestDef = model s c
      u :: T.Text     = r ^. url . coerced
  lift $ logMessage $ "Preparing to send request to URL " <> u :: Step ()
  validatedUrl <- failWith "Error parsing URL" (Req.parseUrl (encodeUtf8 u)) :: Step EitherReq
  bsResponse   <-
    hoist liftIO $ handleExceptT (\(e :: Req.HttpException) -> show e) (runRequest validatedUrl) :: Step
      Req.BsResponse
  now <- liftIO getCurrentTime :: Step UTCTime
  let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
      response              = Response { responseBody = responseMsg, responseDateTime = now }
  lift $ logMessage $ "Response: " <> responseMsg :: Step ()
  lift $ imodify $ responses . at rid . non S.empty %~ (response <|) :: Step ()

-- Helper function called from sendRequest' that performs the actual request
runRequest :: EitherReq -> IO Req.BsResponse
runRequest validatedUrl = Req.runReq Req.defaultHttpConfig $ case validatedUrl of
  Left  (l, opts) -> Req.req Req.GET l Req.NoReqBody Req.bsResponse opts
  Right (r, opts) -> Req.req Req.GET r Req.NoReqBody Req.bsResponse opts
