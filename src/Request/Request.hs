{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Request.Request
  ( sendRequest
  , cancelRequest
  )
where

import           Control.Error
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxMonadState
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Trans.Class      ( lift )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )

import           Brick.BChan                    ( BChan
                                                , writeBChan
                                                )
import           Control.Concurrent.Async       ( Async
                                                , async
                                                , cancel
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( toList )
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Data.String.Conversions        ( cs )
import           Data.Time                      ( diffUTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Language.Haskell.DoNotation
import           Messages.Messages              ( logMessage )
import qualified Network.HTTP.Req              as Req
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( Method(..) )
import           Types.Models.Environment       ( Environment
                                                , Variable
                                                , VariableName(..)
                                                )
import           Types.Models.Header
import           Types.Models.KeyValue          ( isEnabled )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Screen.Optics     ( lastError )
import           Types.Models.Url               ( Url(..) )
import           Types.Monads                   ( IxMonadIO
                                                , iliftIO
                                                )
import           Utils.IfThenElse               ( ifThenElse )
import           Utils.Text                     ( substitute )


-- GADT to hide the scheme
data AnyReq where
  AnyReq ::Req.Url scheme -> Req.Option scheme -> AnyReq

-- This is what is returned from Req.parseUrl
type EitherReq
  = Either (Req.Url 'Req.Http, Req.Option 'Req.Http) (Req.Url 'Req.Https, Req.Option 'Req.Https)

-- We want to treat both Http and Https requests the same, so both sides of EitherReq get
-- combined into AnyReq
eitherReqToAnyReq :: EitherReq -> AnyReq
eitherReqToAnyReq (Left  (u, opts)) = AnyReq u opts
eitherReqToAnyReq (Right (u, opts)) = AnyReq u opts

-- Parse the URL (possibly failing, in which case we log an error and give up), then do the actual
-- HTTP request on a background thread with `async`. This way we don't block Brick's event loop. The
-- result of the request will be sent back into the event loop via a BChan so that the global AppState
-- can be updated appropriately.
-- Since it's possible that the URL cannot be parsed (in case of manually editing the JSON file, etc),
-- this function adds an ExceptT to the top of the monad stack to deal with that case.
sendRequest
  :: (IxMonadState m, IxMonadIO m)
  => RequestDefContext
  -> BChan CustomEvent
  -> m (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag) ()
sendRequest c@(RequestDefContext _ rid) chan = do
  s <- iget
  let r :: RequestDef        = model s c
      e :: Maybe Environment = model s <$> s ^. environmentContext
      vars :: [Variable]     = maybe [] (toList . view variables) e
      u :: Url               = coerce $ substitute vars (r ^. url . coerced)
      errorHandler er = do
        imodify $ screen . lastError ?~ er
        (logMessage . errorDescription) er

  now <- iliftIO getCurrentTime
  logMessage $ "Preparing to send request to URL " <> coerce u

  -- Doing this kind of error handling instead of trying to use ExceptT / MonadError at least for now,
  -- since the two validations are independent so it's only one level of indentation. Could
  -- revisit this at some point.
  case (validateVariables s r, (Req.parseUrl . encodeUtf8 . coerce) u) of
    (Left er, _                ) -> errorHandler er
    (_      , Nothing          ) -> errorHandler $ RequestFailed now "Error parsing URL"
    (Right _, Just validatedUrl) -> do
      asyncResult <- (iliftIO . async)
        $ backgroundSend (eitherReqToAnyReq validatedUrl) c r u chan now
      imodify $ activeRequests . at rid ?~ asyncResult

-- Tries sending the request and constructing the Response model, then sends a custom
-- event into Brick's BChan depending on whether it was a success or failure.
-- Should be run on a background thread.
backgroundSend
  :: AnyReq -> RequestDefContext -> RequestDef -> Url -> BChan CustomEvent -> UTCTime -> IO ()
backgroundSend anyReq c r u chan startTime =
  runExceptT (constructResponse anyReq r u startTime) >>= \case
    Left  e        -> writeBChan chan (ResponseError c e)
    Right response -> writeBChan chan (ResponseSuccess c response)

-- Tries sending the request and constructs the resulting Response model if it was successful.
-- If an HttpException is encountered, will return an error string in ExceptT's error channel.
-- Should be run on a background thread.
constructResponse :: AnyReq -> RequestDef -> Url -> UTCTime -> ExceptT String IO Response
constructResponse anyReq r u startTime = do
  bsResponse <- handleExceptT (\(e :: Req.HttpException) -> show e) (doHttpRequest anyReq r)
  now        <- lift getCurrentTime :: ExceptT String IO UTCTime
  let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
      response = Response { responseBody        = ResponseBody responseMsg
                          , responseStatusCode  = StatusCode $ Req.responseStatusCode bsResponse
                          , responseDateTime    = now
                          , responseMethod      = r ^. method
                          , responseUrl         = u
                          , responseHeaders     = S.filter isEnabled $ r ^. headers
                          , responseRequestBody = r ^. body
                          , responseElapsedTime = diffUTCTime now startTime
                          }
  return response

-- This performs the actual request via the req library's `runReq`. Should be run on a background thread.
doHttpRequest :: AnyReq -> RequestDef -> IO Req.BsResponse
doHttpRequest (AnyReq u opts) r =
  let headerToOpt :: Header -> Req.Option scheme
      headerToOpt h =
          Req.header (encodeUtf8 $ h ^. name . coerced) (encodeUtf8 $ h ^. value . coerced)

      headerOpts :: Req.Option scheme
      headerOpts = foldr (<>) mempty (headerToOpt <$> S.filter isEnabled (r ^. headers))

      allOpts    = opts <> headerOpts
      reqBody    = Req.ReqBodyBs $ cs (r ^. body . coerced :: T.Text)
  in  Req.runReq httpConfig $ case r ^. method of
        Get    -> Req.req Req.GET u Req.NoReqBody Req.bsResponse allOpts
        Post   -> Req.req Req.POST u reqBody Req.bsResponse allOpts
        Patch  -> Req.req Req.PATCH u reqBody Req.bsResponse allOpts
        Put    -> Req.req Req.PUT u reqBody Req.bsResponse allOpts
        Delete -> Req.req Req.DELETE u Req.NoReqBody Req.bsResponse allOpts

cancelRequest
  :: (IxMonadState m, IxMonadIO m)
  => RequestDefContext
  -> Async ()
  -> m (AppState a) (AppState a) ()
cancelRequest (RequestDefContext _ rid) target = do
  imodify $ activeRequests . at rid .~ Nothing
  iliftIO $ cancel target

-- `Req`'s default config, but without throwing an exception on non-2xx status codes,
-- since we want to keep those as normal Responses
httpConfig :: Req.HttpConfig
httpConfig = Req.defaultHttpConfig { Req.httpConfigCheckResponse = \_ _ _ -> Nothing }

-- Make sure that all variables in the RequestDef's URL, headers, and body are all defined in the
-- current environment; if not, update the RequestDef's most recent error.
validateVariables :: AppState 'RequestDefDetailsTag -> RequestDef -> Either RequestError ()
validateVariables s r =
  let varsInEnvironment :: HashSet VariableName = (HashSet.fromList . toList)
        $ maybe S.empty (\e -> view name <$> e ^. variables) (currentEnvironment s)
      unmatchedVariables :: [VariableName] =
          filter (not . flip HashSet.member varsInEnvironment) (allVariables r)
  in  if null unmatchedVariables then Right () else Left (UnmatchedVariables unmatchedVariables)
