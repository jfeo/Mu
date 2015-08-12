module Mu.API.Buffer where

import Mu.API.Types
import Mu.API.Utils

-- | Do some computation specified by the given name string.
--   'Nothing' is returned if no Buffer matches the name.
withBuffer :: String -> Editor -> (Buffer -> a) -> Maybe a
withBuffer name ed f =
  case getBuffer name ed of
    Nothing -> Nothing
    Just b  -> Just $ f b
