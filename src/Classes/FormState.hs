module Classes.FormState where

class FormState a where
  submitValid :: a -> IO ()
