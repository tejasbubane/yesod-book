{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Data.Text (Text)

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person
  { personName :: Text
  , personAge :: Int
  } deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm extra = do
  -- mreq and mopt similar to areq and aopt in applicative forms
  (nameRes, nameView) <- mreq textField "this is not used" Nothing
  (ageRes, ageView) <- mreq intField "neither is this" Nothing
  -- monadic forms return values at each step for us to modify,
  -- unlike applicative which combine them internally
  let personRes = Person <$> nameRes <*> ageRes
  let widget = do
        toWidget
          [lucius|
            ##{fvId ageView} {
              width: 3em;
            }
          |]
        [whamlet|
          #{extra}
          <p>
            Hello, my name is #
            ^{fvInput nameView}
            \ and I am #
            ^{fvInput ageView}
            \ years old. #
            <input type=submit value="Introduce myself">
        |]
  return (personRes, widget)

getHomeR :: Handler Html
getHomeR = do
  ((res, widget), enctype) <- runFormGet personForm
  defaultLayout
    [whamlet|
      <p>Result: #{show res}
      <form enctype=#{enctype}>
        ^{widget}
    |]

main :: IO ()
main = warp 3000 App
