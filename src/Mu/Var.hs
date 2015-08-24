module Mu.Var where

import Mu.Types

changeVar :: String -> Mu -> (String -> String) -> Mu
changeVar k mu f = 
  case k `from` muVars mu of
    Nothing -> mu
    Just v  -> mu { muVars = (v { varValue = f $ varValue v}) : (prune k $ muVars mu)}

withVar :: String -> Mu -> (String -> a) -> Maybe a
withVar k mu f =
  case k `from` muVars mu of 
    Nothing -> Nothing
    Just v  -> Just $ f $ varValue v

-- | Is the variable defined?
hasVar :: String -> Mu -> Bool
hasVar k mu = 
  case k `from` muVars mu of
    Nothing -> False
    Just _  -> True

-- | Get the string value of a variable.
getVar :: String -> Mu -> Maybe String
getVar k mu = withVar k mu id

-- | Set the string value of a variable.
setVar :: String -> String -> Mu -> Mu 
setVar k v mu = 
  case hasVar k mu of
    True  -> changeVar k mu $ \ _ -> v
    False -> mu { muVars = (Var k v) : muVars mu}

-- | Set the string value of a variable, by
--   'show'ing the value.
showVar :: Show a => String -> a -> Mu -> Mu
showVar k v mu = setVar k (show v) mu

-- | Tries to 'read' a variable to some type 'a'.
readVar :: Read a => String -> Mu -> Maybe a
readVar k mu = maybe Nothing id
             $ withVar k mu $ \s -> 
               case reads s of
                 []    -> Nothing
                 (v:_) -> Just $ fst v



