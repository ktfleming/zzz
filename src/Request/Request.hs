{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.Request
  ( sendRequest,
    cancelRequest,
  )
where

import Brick.BChan
  ( BChan,
    writeBChan,
  )
import qualified Config
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Error
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Class (lift)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Sequence as Seq
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )
import Data.Time
import Data.Time.Clock (UTCTime)
import Messages.Messages (logMessage)
import qualified Network.HTTP.Req as Req
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Methods (Method (..))
import Types.Models.Environment
  ( Environment,
    Variable,
    VariableName (..),
  )
import Types.Models.Header
import Types.Models.KeyValue (isEnabled)
import Types.Models.RequestDef
import Types.Models.Response
import Types.Models.Screen
import Types.Models.Screen.Optics (lastError)
import Types.Models.Url (Url (..))
import Types.Time
import Utils.Text

data AnyReq where
  AnyReq :: Req.Url scheme -> Req.Option scheme -> AnyReq

-- This is what is returned from Req.parseUrl
type EitherReq =
  Either (Req.Url 'Req.Http, Req.Option 'Req.Http) (Req.Url 'Req.Https, Req.Option 'Req.Https)

-- We want to treat both Http and Https requests the same, so both sides of EitherReq get
-- combined into AnyReq
eitherReqToAnyReq :: EitherReq -> AnyReq
eitherReqToAnyReq (Left (u, opts)) = AnyReq u opts
eitherReqToAnyReq (Right (u, opts)) = AnyReq u opts

-- Parse the URL (possibly failing, in which case we log an error and give up), then do the actual
-- HTTP request on a background thread with `async`. This way we don't block Brick's event loop. The
-- result of the request will be sent back into the event loop via a BChan so that the global AppState
-- can be updated appropriately.
sendRequest ::
  (MonadReader Config.AppConfig m, MonadIO m) =>
  RequestDefContext ->
  BChan CustomEvent ->
  AppState 'RequestDefDetailsTag ->
  m (AppState 'RequestDefDetailsTag)
sendRequest c@(RequestDefContext _ rid) chan s = do
  let r :: RequestDef = model s c
      e :: Maybe Environment = model s <$> s ^. environmentContext
      vars :: [Variable] = maybe [] (toList . view variables) e
      finalUrl :: Url = substitute vars (r ^. url)
      -- Handling an error means logging it and updating the `lastError` field on the Screen
      errorHandler :: (MonadReader Config.AppConfig m, MonadIO m) => RequestError -> m (AppState 'RequestDefDetailsTag)
      errorHandler er = do
        logMessage (errorDescription er)
        pure $ s & screen . lastError ?~ er
  now <- liftIO getCurrentTime
  -- Doing this kind of error handling instead of trying to use ExceptT / MonadError at least for now,
  -- since the two validations are independent so it's only one level of indentation. Could
  -- revisit this at some point.
  case (validateVariables s r, (Req.parseUrl . encodeUtf8 . coerce) finalUrl) of
    (Left er, _) -> errorHandler er
    (_, Nothing) -> do
      tz <- asks (view Config.timeZone)
      errorHandler $ RequestFailed (AppTime (utcToZonedTime tz now)) "Error parsing URL"
    (Right _, Just validatedUrl) -> do
      let asyncRequest :: IO (Async ()) =
            Async.async $ backgroundSend (eitherReqToAnyReq validatedUrl) c r finalUrl chan now vars
      asyncResult <- liftIO $ AppAsync <$> asyncRequest
      pure $ s & activeRequests . at rid ?~ asyncResult

-- Tries sending the request and constructing the Response model, then sends a custom
-- event into Brick's BChan depending on whether it was a success or failure.
-- Should be run on a background thread.
backgroundSend ::
  AnyReq -> RequestDefContext -> RequestDef -> Url -> BChan CustomEvent -> UTCTime -> [Variable] -> IO ()
backgroundSend anyReq c r u chan startTime vars =
  runExceptT (constructResponse anyReq r u startTime vars) >>= \result -> writeBChan chan $ case result of
    Left e -> ResponseError c e
    Right response -> ResponseSuccess c response

-- Tries sending the request and constructs the resulting Response model if it was successful.
-- If an HttpException is encountered, will return an error string in ExceptT's error channel.
-- Should be run on a background thread.
constructResponse :: AnyReq -> RequestDef -> Url -> UTCTime -> [Variable] -> ExceptT String IO Response
constructResponse anyReq r u startTime vars = do
  bsResponse <- handleExceptT (\(e :: Req.HttpException) -> show e) (doHttpRequest anyReq r vars)
  now <- lift getCurrentTime :: ExceptT String IO UTCTime
  let responseMsg :: Text = (decodeUtf8 . Req.responseBody) bsResponse
  pure
    Response
      { responseBody = ResponseBody responseMsg,
        responseStatusCode = StatusCode $ Req.responseStatusCode bsResponse,
        responseDateTime = now,
        responseMethod = r ^. method,
        responseUrl = u,
        responseHeaders = fmap (substituteHeader vars) . Seq.filter isEnabled $ r ^. headers,
        responseRequestBody = substitute vars (r ^. body),
        responseElapsedTime = diffUTCTime now startTime
      }

-- This performs the actual request via the req library's `runReq`. Should be run on a background thread.
doHttpRequest :: AnyReq -> RequestDef -> [Variable] -> IO Req.BsResponse
doHttpRequest (AnyReq u opts) r vars =
  let headerToOpt :: Header -> Req.Option scheme
      headerToOpt h =
        let encodeAndSub = encodeUtf8 . substitute vars
         in Req.header (encodeAndSub $ h ^. name . coerced) (encodeAndSub $ h ^. value . coerced)
      headerOpts :: Req.Option scheme
      headerOpts = (mconcat . toList) (headerToOpt <$> Seq.filter isEnabled (r ^. headers))
      allOpts = opts <> headerOpts
      reqBody = Req.ReqBodyBs $ cs (substitute vars (r ^. body . coerced) :: Text)
   in Req.runReq httpConfig $ case r ^. method of
        Get -> Req.req Req.GET u Req.NoReqBody Req.bsResponse allOpts
        Post -> Req.req Req.POST u reqBody Req.bsResponse allOpts
        Patch -> Req.req Req.PATCH u reqBody Req.bsResponse allOpts
        Put -> Req.req Req.PUT u reqBody Req.bsResponse allOpts
        Delete -> Req.req Req.DELETE u Req.NoReqBody Req.bsResponse allOpts

cancelRequest ::
  MonadIO m =>
  RequestDefContext ->
  Async () ->
  AppState a ->
  m (AppState a)
cancelRequest (RequestDefContext _ rid) target s = do
  liftIO $ Async.cancel target
  pure $ s & activeRequests . at rid .~ Nothing

-- `Req`'s default config, but without throwing an exception on non-2xx status codes,
-- since we want to keep those as normal Responses
httpConfig :: Req.HttpConfig
httpConfig = Req.defaultHttpConfig {Req.httpConfigCheckResponse = \_ _ _ -> Nothing}

-- Make sure that all variables in the RequestDef's URL, headers, and body are all defined in the
-- current environment; if not, update the RequestDef's most recent error.
validateVariables :: AppState 'RequestDefDetailsTag -> RequestDef -> Either RequestError ()
validateVariables s r =
  let varsInEnvironment :: HashSet VariableName =
        (HashSet.fromList . toList) $
          maybe Seq.empty (\e -> view name <$> e ^. variables) (currentEnvironment s)
      unmatchedVariables :: [VariableName] =
        filter (not . flip HashSet.member varsInEnvironment) (allVariables r)
   in if null unmatchedVariables then Right () else Left (UnmatchedVariables unmatchedVariables)
