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
import           Network.HTTP.Req
import           Types.AppState                 ( AppState
                                                , lookupRequestDefinition
                                                )
import           Types.Models.Url               ( Url(..) )
--import Types.Methods

import           Messages.Messages              ( logMessage )

type EitherReq
  = Either
      (Network.HTTP.Req.Url 'Http, Option 'Http)
      (Network.HTTP.Req.Url 'Https, Option 'Https)

sendRequest
  :: AppState -> RequestDefinitionContext -> ExceptT String IO AppState
sendRequest s c =
  let r :: RequestDefinition = lookupRequestDefinition s c
      myUrl :: T.Text        = r ^. url . coerced
  in  do
        s' <- liftIO
          $ logMessage s ("Preparing to send request to URL " <> myUrl)
        validatedUrl :: EitherReq <- failWith "Error parsing URL"
          $ parseUrl (encodeUtf8 myUrl)
        response :: BsResponse <- handleExceptT
          (\(_ :: HttpException) -> "Exception!")
          (sendRequest' validatedUrl)
        let responseMsg :: T.Text = (decodeUtf8 . responseBody) response
        liftIO $ logMessage s' ("Response: " <> responseMsg)


sendRequest' :: EitherReq -> IO BsResponse
sendRequest' validatedUrl = runReq defaultHttpConfig $ case validatedUrl of
  Left  (l, _) -> req GET l NoReqBody bsResponse mempty
  Right (r, _) -> req GET r NoReqBody bsResponse mempty
