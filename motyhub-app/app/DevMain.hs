module DevMain where

import App

reload :: IO ()
reload = do
  putStrLn "reloading..."
  App.startApp
