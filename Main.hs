{-# LANGUAGE FlexibleInstances #-}
import Control.Monad (forM)
import System.Environment (getArgs)

import Vk (getFriends, getUser, Profile(..))

class ToDot a where
  toDot :: a -> String

instance ToDot Profile where
  toDot p = (show . uid) p ++ " [ label = \"" ++ firstName p ++ " " ++ lastName p ++ "\" ];"

instance ToDot ((,) Profile Profile) where
  toDot (Profile _ _ uid, Profile _ _ uid') =
    show uid ++ " -- " ++ show uid' ++ ";"

main = do
  target <- read <$> head <$> getArgs
  fs <- getUser target >>= getFriends
  putStrLn "graph {"
  mapM (putStrLn . toDot) fs
  forM fs $ \x -> do
    adj <- filter ((< uid x) . uid) <$> filter (flip elem fs) <$> getFriends x
    mapM (putStrLn . toDot . (,) x) adj
  putStrLn "}"
