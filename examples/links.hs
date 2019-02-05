{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
/page3 Page3R GET
|]

instance Yesod Links

-- @{Route} creates a link (they are type-safe!)
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<a href=@{Page1R}>Go to page1!|]

getPage1R :: Handler Html
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page2!|]

getPage2R :: Handler Html
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go Home!|]

-- Example of JSON Response
getPage3R :: HandlerFor Links Value
getPage3R = return $ object ["msg" .= "Hello World"]

main :: IO ()
main = warp 3000 Links
