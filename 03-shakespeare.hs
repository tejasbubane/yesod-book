{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (HtmlUrlI18n, ihamlet)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl Home _ = "/home"
renderUrl Time _ = "/time"
renderUrl Stylesheet _ = "/style.css"

data Msg = Hello | Apples Int

-- I18n
renderEnglish :: Msg -> Text
renderEnglish Hello = "Hello"
renderEnglish (Apples 0) = "You did not buy any apples."
renderEnglish (Apples 1) = "You bought 1 apple."
renderEnglish (Apples i) = T.concat ["You bought ", T.pack $ show i, " apples."]

template :: Int -> HtmlUrlI18n Msg MyRoute
template count = [ihamlet|
$doctype 5
<html>
  <head>
    <title>i18n
  <body>
    <h1>_{Hello}
    <p>_{Apples count}
|]

main :: IO ()
main = putStrLn $ renderHtml
  $ (template 5) (toHtml . renderEnglish) renderUrl
