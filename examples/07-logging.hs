{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Yesod
import Control.Exception (IOException, try)
import Control.Monad (when)
data App = App
instance Yesod App where
  -- shouldLogIO function controls which messages get logged
  shouldLogIO App src level = return $
    True -- for dev
    -- level == LevelWarn || level == LevelError -- for prod

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
  $logDebug "Trying to read data file"
  edata <- liftIO $ try $ readFile "datafile.txt"
  -- use liftIO inside Handler monad to do any arbitrary IO

  case edata :: Either IOException String of
    Left e -> do
      $logError $ "could not read datafile.txt"
      defaultLayout [whamlet|An error occurred|]
    Right str -> do
      $logInfo "Reading of data file succeeded"
      let ls = lines str
      when (length ls < 5) $ $logWarn "Less than 5 lines found"
      defaultLayout
        [whamlet|
          <ol>
            $forall l <- ls
              <li>#{l}
        |]

main :: IO ()
main = warp 3000 App
