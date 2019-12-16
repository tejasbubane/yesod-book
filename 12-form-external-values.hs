{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

newtype UserId = UserId Int deriving Show

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)

data Blog = Blog
  { blogTitle :: Text
  , blogContents :: Textarea
  , blogUser :: UserId
  , blogPosted :: UTCTime
  } deriving Show

form :: UserId -> Form Blog
form userId = renderDivs $ Blog
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Contents" Nothing
  <*> pure userId
  <*> lift (liftIO getCurrentTime)

-- for external values in applicative context
-- use pure for embedding values
-- use lift to lift arbitrary IO Monads into applicative context

getHomeR :: Handler Html
getHomeR = do
  let userId = UserId 5
  -- userId is hard-coded here for example sake, but this will come from authentication (current-user)
  ((res, widget), enctype) <- runFormPost $ form userId
  defaultLayout
    [whamlet|
      <p>Previous result: #{show res}
      <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{widget}
        <input type=submit>
    |]

postHomeR :: Handler Html
postHomeR = getHomeR -- example to reuse the same handler (form handling)

main :: IO ()
main = warp 3000 App
