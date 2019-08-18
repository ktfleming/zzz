{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.Request
  ( sendRequest
  )
where

import           Types.Models.RequestDef

import           Brick                          ( EventM )
import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Morph            ( hoist )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State.Lazy ( StateT
                                                , get
                                                , modify
                                                )
import           Data.HashMap.Strict            ( HashMap )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time                      ( getCurrentTime )
import           Messages.Messages              ( logMessage )
import qualified Network.HTTP.Req              as Req
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Classes.HasId            ( model )
import           Types.Models.Id                ( RequestDefId )
import           Types.Models.Response
import           Types.Models.Url               ( Url(..) )


type EitherReq
  = Either
      (Req.Url 'Req.Http, Req.Option 'Req.Http)
      (Req.Url 'Req.Https, Req.Option 'Req.Https)

-- This is the function called from the event handler; it uses the same monad stack that all
-- the event handlers use/require. But our main function that sends the request has an extra
-- ExceptT layer that the event handlers don't have, so we have to handle the error (if present)
-- by logging it, and then removing the ExceptT layer.
sendRequest :: RequestDefContext -> StateT AppState (EventM Name) ()
sendRequest c = do
  result <- runExceptT $ sendRequest' c
  case result of
    Left  msg -> logMessage (T.pack msg)
    Right ()  -> return ()

-- Since the HTTP request can throw an exception, this function adds an ExceptT to the top of the monad stack to
-- deal with that case.
sendRequest'
  :: RequestDefContext -> ExceptT String (StateT AppState (EventM Name)) ()
sendRequest' c@(RequestDefContext _ rid) = do
  s <- lift get
  let r :: RequestDef = model s c
      u :: T.Text     = r ^. url . coerced
  lift $ logMessage $ "Preparing to send request to URL " <> u
  validatedUrl <- failWith "Error parsing URL" (Req.parseUrl (encodeUtf8 u))
  bsResponse   <- hoist liftIO $ handleExceptT
    (\(e :: Req.HttpException) -> show e)
    (runRequest validatedUrl)
  now <- liftIO getCurrentTime
  let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
      response =
        Response { responseBody = responseMsg, responseDateTime = now }
  lift $ logMessage $ "Response: " <> responseMsg

  -- Seems like I have to assert the type of this lens for it to work
  let responseLens =
        (responses . coerced) :: Lens'
            AppState
            (HashMap RequestDefId (Seq Response))

  lift $ modify $ responseLens . at rid . non S.empty %~ (response <|)

-- Helper function called from sendRequest' that performs the actual request
runRequest :: EitherReq -> IO Req.BsResponse
runRequest validatedUrl =
  Req.runReq Req.defaultHttpConfig $ case validatedUrl of
    Left  (l, opts) -> Req.req Req.GET l Req.NoReqBody Req.bsResponse opts
    Right (r, opts) -> Req.req Req.GET r Req.NoReqBody Req.bsResponse opts
