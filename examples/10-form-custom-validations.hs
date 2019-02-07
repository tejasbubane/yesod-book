{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Control.Applicative ((<$>), (<*>))
import Yesod.Form.Jquery
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

data App = App
instance Yesod App
instance YesodJquery App

mkYesod "App" [parseRoutes|
/ HomeR GET
/car CarR POST
|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Car = Car
  {
    carModel :: Text
  , carYear :: Int
  , carColor :: Maybe Text
  }
  deriving (Eq, Show)

carAForm :: AForm Handler Car
carAForm = Car
  <$> areq textField "Model" Nothing
  <*> areq carYearField "Year" Nothing -- instead of intField use custom for validations
  <*> aopt textField "Color" Nothing
  where
    errorMessage :: Text
    errorMessage = "Your car is too old, get a new one!"
    -- checkM for monadic check
    carYearField = checkM inFuture $ check validateYear intField
    -- 1. Validate that car is not very old
    validateYear y
      | y < 1990  = Left errorMessage
      | otherwise = Right y
    -- This pattern is very common hence shorthand:
    -- carYearField = checkBool (>= 1990) errorMessage intField

    -- 2. Validate year is not in future
    inFuture y = do
      thisYear <- liftIO getCurrentYear -- perform IO inside this monad
      return $ if y >= thisYear
        then Left ("You have a time machine!" :: Text)
        else Right y
    -- Getting current year from current time is an IO operation
    getCurrentYear :: IO Int
    getCurrentYear = do
      now <- getCurrentTime
      let (year, _, _) = toGregorian . utctDay $ now
      return $ fromInteger year

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable carAForm

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost carForm
  defaultLayout
    [whamlet|
      <form method=post action=@{CarR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]

postCarR :: Handler Html
postCarR = do
  ((result, widget), enctype) <- runFormPost carForm
  case result of
    FormSuccess car -> defaultLayout [whamlet|<p>#{show car}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, lets try again
        <form method=post action=@{CarR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]

main :: IO ()
main = warp 3000 App
