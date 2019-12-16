{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Data.Time (getCurrentTime)

data App = App

-- App Server will send requests from /wiki/home to this app on /home
mkYesod "App" [parseRoutes|
/home HomeR GET
|]

instance Yesod App where
  -- This is for rendering only - use /wiki in rendered URLs
  approot = ApprootStatic "http://localhost:3000/wiki" -- no trailing slash here

  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent contents
    mmsg <- getMessage
    withUrlRenderer [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{title}
          ^{headTags}
        <body>
          $maybe msg <- mmsg
            <div #message>#{msg}
          ^{bodyTags}
    |]
  -- errorHandler NotFound - Overload the errorHandler function for custom error pages
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "Request page not located"
    toWidget [hamlet|
      <h1>Not Found
      <p>We apologize for the inconvenience, but the requested page could not be found
    |]
  errorHandler other = defaultErrorHandler other


getHomeR :: Handler Html
getHomeR = do
  now <- liftIO getCurrentTime
  setMessage $ toHtml $ "You previously visited at: " ++ show now
  defaultLayout [whamlet|<p>Try refreshing..|]

main :: IO ()
main = warp 3000 App
