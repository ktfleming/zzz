{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.Request
  ( sendRequest
  )
where

import           Types.Models.RequestDefinition

import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Network.HTTP.Req              as Req
import           Types.AppState
import           Types.Models.Response
import           Types.Models.Url               ( Url(..) )
--import Types.Methods

import           Data.HashMap.Strict            ( HashMap )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.Time                      ( getCurrentTime )
import           Messages.Messages              ( logMessage )
import           Types.Models.Id                ( RequestDefinitionId )


type EitherReq
  = Either
      (Req.Url 'Req.Http, Req.Option 'Req.Http)
      (Req.Url 'Req.Https, Req.Option 'Req.Https)

sendRequest
  :: AppState -> RequestDefinitionContext -> ExceptT String IO AppState
sendRequest s c@(RequestDefinitionContext _ rid) =
  let r :: RequestDefinition = lookupRequestDefinition s c
      myUrl :: T.Text        = r ^. url . coerced
  in  do
        -- These s' and s'' AppStates represent the state with added log messages.
        s' <- liftIO
          $ logMessage s ("Preparing to send request to URL " <> myUrl)
        validatedUrl :: EitherReq <- failWith "Error parsing URL"
          $ Req.parseUrl (encodeUtf8 myUrl)
        bsResponse :: Req.BsResponse <- handleExceptT
          (\(e :: Req.HttpException) -> show e)
          (sendRequest' validatedUrl)
        now <- liftIO getCurrentTime
        let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
            response :: Response =
              Response { responseBody = responseMsg, responseDateTime = now }
        s'' <- liftIO $ logMessage s' ("Response: " <> responseMsg)

        -- Seems like I have to assert the type of this lens for it to work
        let responseLens =
              (responses . coerced) :: Lens'
                  AppState
                  (HashMap RequestDefinitionId (Seq Response))

        return $ s'' & responseLens . at rid . non S.empty %~ (response <|)

sendRequest' :: EitherReq -> IO Req.BsResponse
sendRequest' validatedUrl =
  Req.runReq Req.defaultHttpConfig $ case validatedUrl of
    Left  (l, opts) -> Req.req Req.GET l Req.NoReqBody Req.bsResponse opts
    Right (r, opts) -> Req.req Req.GET r Req.NoReqBody Req.bsResponse opts
