{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: HandlerFor App Html
getHomeR = defaultLayout $ do
  setTitle "My Page Title"
  toWidget [lucius| h1 { color: green; } |]
  addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
  toWidget
    [julius|
      $(function() {
        $("h1").click(function() {
          alert("You clicked on the heading!");
        });
      });
    |]
  toWidgetHead [hamlet| <meta name=keywords content="some sample keywords"> |]
  toWidget [hamlet| <h1>Here's one way of including content |] -- note no need of closing tags in hamlet!
  [whamlet|<h2>Here's another|] -- whamlet converts hamlet to widget
  toWidgetBody
    [julius| alert("This is included in the body itself"); |]

main :: IO ()
main = warp 3000 App
