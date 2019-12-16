{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Time (Day)
import Yesod.Form.Jquery

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/person PersonR POST
|]

instance Yesod App
instance YesodJquery App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person
  { personName :: Text
  , personBirthday :: Day
  , personFavoriteColor :: Maybe Text
  , personEmail :: Text
  , personWebsite :: Maybe Text
  }
  deriving (Eq, Show)

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
  <$> areq textField "Name" Nothing
  -- 3 args for areq: field, settings(name, id, etc), optional default value
  <*> areq (jqueryDayField def
           { jdsChangeYear = True -- give year dropdown
           , jdsYearRange = "1900:-5" -- 1900 till 5yrs ago
           }) "Birthday" Nothing
  <*> aopt textField "Favourite color" Nothing
  <*> areq emailField "Email address" Nothing
  <*> aopt urlField "Website" Nothing
  -- `areq` is for required field, `aopt` for optional where `a` stands for applicative
  -- We combine form fields using applicative - allows combining errors
  -- then renderDivs takes care of converting it to monadic form (MForm in type signature)

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost personForm
  defaultLayout
    [whamlet|
      <p> The widget generated contains only the contents of the form, not the form tag itself. So..
      <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button.
        <button>Submit
    |]

-- Handle the form submission post request
postPersonR :: Handler Html
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form method=post action=@{PersonR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]

main :: IO ()
main = warp 3000 App
