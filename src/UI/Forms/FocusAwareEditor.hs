{-# LANGUAGE RankNTypes #-}

module UI.Forms.FocusAwareEditor
  ( focusAwareEditField,
  )
where

import Brick
  ( BrickEvent (..),
    Widget,
    vBox,
  )
import Brick.Forms
  ( FormField (..),
    FormFieldState (..),
  )
import Brick.Widgets.Edit
  ( applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
  )
import Control.Lens
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Zipper as Z

-- This is copied from Brick's `editField`, but takes an extra function that
-- determines how to render the field when it's not focused. This allows for
-- things like syntax highlighting on Header forms when they're not focused --
-- having syntax highlighting even while editing seems to be pretty difficult
-- because it would involve stitching together various Widgets with different
-- attributes that change while the user is editing, plus handling cursor
-- movement...so this is my compromise for now.
focusAwareEditField ::
  (Ord n, Show n) =>
  Lens' s a ->
  n ->
  Maybe Int ->
  (a -> Text) ->
  ([Text] -> Maybe a) ->
  ([Text] -> Widget n) ->
  (Widget n -> Widget n) ->
  Maybe ([Text] -> Widget n) -> -- This is the `readOnlyRender` function that I added
  s ->
  FormFieldState s e n
focusAwareEditField stLens n limit ini val renderText wrapEditor readOnlyRender initialState =
  let initVal = applyEdit gotoEnd $ editor n limit initialText
      gotoEnd =
        let ls = T.lines initialText
            pos = (length ls - 1, T.length (last ls))
         in if null ls then id else Z.moveCursor pos
      initialText = ini $ initialState ^. stLens
      handleEvent (VtyEvent e) ed = handleEditorEvent e ed
      handleEvent _ ed = pure ed
      defaultRender focused e = wrapEditor $ renderEditor renderText focused e
   in FormFieldState
        { formFieldState = initVal,
          formFields =
            [ FormField
                n
                (val . getEditContents)
                True
                ( \focused e ->
                    if focused -- This is the only part that's changed from Brick
                      then defaultRender True e
                      else case readOnlyRender of
                        Just f -> f $ getEditContents e
                        Nothing -> defaultRender False e
                )
                handleEvent
            ],
          formFieldLens = stLens,
          formFieldRenderHelper = id,
          formFieldConcat = vBox
        }
