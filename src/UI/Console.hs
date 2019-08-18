{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Console
  ( console
  , toggleConsole
  )
where

import           Brick                          ( Widget
                                                , txt
                                                )
import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , iput
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Utils.IxState                  ( submerge )

console :: Seq Message -> Widget Name
console ms = txt $ T.intercalate "\n" $ coerce (toList ms)

toggleConsole :: Monad m => IxStateT m AnyAppState AnyAppState ()
toggleConsole = iget >>>= \(AnyAppState s) ->
  submerge $ iput $ s & consoleVisible . coerced %~ not
