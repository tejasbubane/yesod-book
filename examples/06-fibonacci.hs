{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data App = App
instance Yesod App

-- type safety at boundary for Natural numbers using pathpieces
newtype Natural = Natural Int deriving (Eq, Show, Read)
instance PathPiece Natural where
  toPathPiece (Natural i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, ""):_
        | i < 1 -> Nothing
        | otherwise -> Just $ Natural i
      [] -> Nothing

mkYesod "App" [parseRoutes|
/fib/#Natural FibR GET
|]
-- Routes with params are like
-- /user/#Text /comment/#Int or in this case with custom data type as well

-- route params are just arguments to handler functions!
getFibR :: Natural -> Handler Text -- return text/plain
getFibR (Natural n) = return $
  case fib n of
    Just x -> T.pack $ show x
    Nothing -> "Number should be greater than 0."

fib :: Int -> Maybe Int
fib 1 = Just $ 1
fib 2 = Just $ 1
fib n = Just $ (fromMaybe 0 $ fib (n - 1)) + (fromMaybe 0 $ fib (n - 2))

main :: IO ()
main = warp 3000 App
