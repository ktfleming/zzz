{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Request.Request
  ( sendRequest
  )
where

import           Types.Models.RequestDefinition

import           Brick                          ( EventM )
import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Morph            ( hoist )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State.Lazy ( StateT(..)
                                                , get
                                                , modify
                                                , runStateT
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
import           Types.Models.Id                ( RequestDefinitionId )
import           Types.Models.Response
import           Types.Models.Url               ( Url(..) )


type EitherReq
  = Either
      (Req.Url 'Req.Http, Req.Option 'Req.Http)
      (Req.Url 'Req.Https, Req.Option 'Req.Https)

-- This is the function called from the event handler; it uses the same monad stack that all
-- the event handlers use/require. But our main function that sends the request has an extra
-- ExceptT layer that the event handlers don't have, so we have to handle the error (if present)
-- by logging it, and then removing the ExceptT layer. So this function basically just
-- 1. calls the "real" function, sendRequest', that performs the request in the monad stack that has ExceptT
-- 2. constructs a new stack without ExceptT by unwrapping the provided stack with runStateT and runExceptT.
--    Once we reach the level where we can match on the Either...
--      a. If an error is present, send the initial state and the error to the `logMessage` function
--         which will take care of the logging. The state doesn't otherwise change (there's no response to record)
--      b. If a (successfully modified) AppState is present, just return it and discard the initial state.
--         This returned AppState will have additional logs as well as the response recorded.
sendRequest :: RequestDefinitionContext -> StateT AppState (EventM Name) ()
sendRequest c = StateT $ \s ->
  let unwrapped = runExceptT $ runStateT (sendRequest' c) s
  in  do
        stateOrError :: Either String ((), AppState) <- unwrapped
        case stateOrError of
          Left  msg     -> (runStateT $ logMessage (T.pack msg)) s
          Right okState -> return okState

-- The "real" function that performs the request in a monad stack that includes ExceptT
sendRequest'
  :: RequestDefinitionContext
  -> StateT AppState (ExceptT String (EventM Name)) ()
sendRequest' c@(RequestDefinitionContext _ rid) = do
  s <- get
  let r :: RequestDefinition = model s c
      u :: T.Text            = r ^. url . coerced
  logMessage $ "Preparing to send request to URL " <> u
  validatedUrl <- getRequest u
  bsResponse   <- doSend validatedUrl
  now          <- liftIO getCurrentTime
  let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
      response =
        Response { responseBody = responseMsg, responseDateTime = now }
  logMessage $ "Response: " <> responseMsg

  -- Seems like I have to assert the type of this lens for it to work
  let responseLens =
        (responses . coerced) :: Lens'
            AppState
            (HashMap RequestDefinitionId (Seq Response))

  modify $ responseLens . at rid . non S.empty %~ (response <|)

-- These steps are used in `sendRequest'`, they're broken up into multiple functions
-- to make the types clear.
getRequest :: T.Text -> StateT AppState (ExceptT String (EventM Name)) EitherReq
getRequest t =
  let maybeReq :: Maybe EitherReq = Req.parseUrl (encodeUtf8 t)
      reqOrError :: ExceptT String (EventM Name) EitherReq =
          failWith "Error parsing URL" maybeReq
  in  lift reqOrError

doSend
  :: EitherReq -> StateT AppState (ExceptT String (EventM Name)) Req.BsResponse
doSend validatedUrl = lift $ hoist liftIO (handled validatedUrl)

handled :: EitherReq -> ExceptT String IO Req.BsResponse
handled validatedUrl =
  handleExceptT (\(e :: Req.HttpException) -> show e) (runRequest validatedUrl)

runRequest :: EitherReq -> IO Req.BsResponse
runRequest validatedUrl =
  Req.runReq Req.defaultHttpConfig $ case validatedUrl of
    Left  (l, opts) -> Req.req Req.GET l Req.NoReqBody Req.bsResponse opts
    Right (r, opts) -> Req.req Req.GET r Req.NoReqBody Req.bsResponse opts
