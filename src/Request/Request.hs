{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.Request
  ( sendRequest
  , cancelRequest
  )
where

import           Brick                          ( EventM )
import           Control.Error
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
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
import           Types.Brick.Name               ( Name )
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( Method(..) )
import           Types.Models.Header
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )


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

-- This is the function called from the event handler; it uses the same monad stack that all
-- the event handlers use/require. But our main function that sends the request has an extra
-- ExceptT layer that the event handlers don't have, so we have to handle the error (if present)
-- by logging it, and then removing the ExceptT layer.
sendRequest
  :: RequestDefContext
  -> BChan CustomEvent
  -> IxStateT
       (EventM Name)
       (AppState 'RequestDefDetailsTag)
       (AppState 'RequestDefDetailsTag)
       ()
sendRequest c chan = do
  result <- runExceptT $ sendRequest' c chan
  case result of
    Left  msg -> logMessage (T.pack msg)
    Right _   -> return ()

-- Seems like the sendRequest' function needs some help with type annotations, so this alias will make it
-- not so verbose.
type Step a
  = ExceptT
      String
      (IxStateT (EventM Name) (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag))
      a

-- Parse the URL (possibly failing, in which case we log an error and give up), then do the actual
-- HTTP request on a background thread with `async`. This way we don't block Brick's event loop. The
-- result of the request will be sent back into the event loop via a BChan so that the global AppState
-- can be updated appropriately.
-- Since it's possible that the URL cannot be parsed (in case of manually editing the JSON file, etc),
-- this function adds an ExceptT to the top of the monad stack to deal with that case.
sendRequest' :: RequestDefContext -> BChan CustomEvent -> Step ()
sendRequest' c@(RequestDefContext _ rid) chan = do
  s <- lift iget :: Step (AppState 'RequestDefDetailsTag)
  let r :: RequestDef = model s c
      u :: T.Text     = r ^. url . coerced
  lift $ logMessage $ "Preparing to send request to URL " <> u :: Step ()
  validatedUrl <- failWith "Error parsing URL" (Req.parseUrl (encodeUtf8 u)) :: Step EitherReq
  startTime    <- liftIO getCurrentTime :: Step UTCTime
  asyncResult  <-
    (liftIO . async) $ backgroundSend (eitherReqToAnyReq validatedUrl) c r chan startTime :: Step
      (Async ())
  lift $ imodify $ activeRequests . at rid ?~ asyncResult :: Step ()
  return ()

-- Tries sending the request and constructing the Response model, then sends a custom
-- event into Brick's BChan depending on whether it was a success or failure.
-- Should be run on a background thread.
backgroundSend :: AnyReq -> RequestDefContext -> RequestDef -> BChan CustomEvent -> UTCTime -> IO ()
backgroundSend anyReq c r chan startTime =
  runExceptT (constructResponse anyReq r startTime) >>= \case
    Left  e        -> writeBChan chan (ResponseError c e)
    Right response -> writeBChan chan (ResponseSuccess c response)

-- Tries sending the request and constructs the resulting Response model if it was successful.
-- If an HttpException is encountered, will return an error string in ExceptT's error channel.
-- Should be run on a background thread.
constructResponse :: AnyReq -> RequestDef -> UTCTime -> ExceptT String IO Response
constructResponse anyReq r startTime = do
  bsResponse <- handleExceptT (\(e :: Req.HttpException) -> show e) (doHttpRequest anyReq r)
  now        <- lift getCurrentTime :: ExceptT String IO UTCTime
  let responseMsg :: T.Text = (decodeUtf8 . Req.responseBody) bsResponse
      response              = Response { responseBody        = ResponseBody responseMsg
                                       , responseDateTime    = now
                                       , responseMethod      = r ^. method
                                       , responseUrl         = r ^. url
                                       , responseHeaders = S.filter isHeaderEnabled $ r ^. headers
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
      headerOpts = foldr (<>) mempty (headerToOpt <$> S.filter isHeaderEnabled (r ^. headers))

      allOpts    = opts <> headerOpts
      reqBody    = Req.ReqBodyBs $ cs (r ^. body . coerced :: T.Text)
  in  Req.runReq Req.defaultHttpConfig $ case r ^. method of
        Get   -> Req.req Req.GET u Req.NoReqBody Req.bsResponse allOpts
        Post  -> Req.req Req.POST u reqBody Req.bsResponse allOpts
        Patch -> Req.req Req.PATCH u reqBody Req.bsResponse allOpts
        _     -> error "TODO"

cancelRequest
  :: MonadIO m => RequestDefContext -> Async () -> IxStateT m (AppState a) (AppState a) ()
cancelRequest (RequestDefContext _ rid) target = do
  imodify $ activeRequests . at rid .~ Nothing
  liftIO $ cancel target
