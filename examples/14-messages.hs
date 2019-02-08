{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/set-message SetMessageR POST
|]

-- Message is used to show some response after post-redirect
-- like flash messages in Ruby on Rails :)
-- Put message in main layout so it is available everywhere
instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    mmsg <- getMessage -- message goes off after reading, not seen in next request
    withUrlRenderer
      [hamlet|
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle pc}
            ^{pageHead pc}
          <body>
            $maybe msg <- mmsg
              <p>Your message was: #{msg}
            ^{pageBody pc}
      |]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
  [whamlet|
    <form method=post action=@{SetMessageR}>
      My message is: #
      <input type=text name=message>
      <button>Go
  |]

postSetMessageR :: Handler ()
postSetMessageR = do
  msg <- runInputPost $ ireq textField "message"
  setMessage $ toHtml msg -- set message and redirect, message will be available in redirected handler
  redirect HomeR

main :: IO ()
main = warp 3000 App
