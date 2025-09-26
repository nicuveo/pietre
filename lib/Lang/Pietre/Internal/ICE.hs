module Lang.Pietre.Internal.ICE where

import "this" Prelude

import GHC.Stack


reportICE :: HasCallStack => String -> String -> [String] -> a
reportICE context message details =
  error $ unlines $
    [ "*** INTERNAL COMPILER ERROR ***"
    , "while attempting to: " ++ context
    , "error: " ++ message
    ] ++ details

unimplemented :: HasCallStack => a
unimplemented =
  error "NOT IMPLEMENTED YET"
