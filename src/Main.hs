{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import qualified Chess as C

main = scotty 3000 $ do
	get "/" $ do
		html "Hello World!"
