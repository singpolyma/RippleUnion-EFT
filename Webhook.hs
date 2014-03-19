{-# LANGUAGE CPP #-}
module Webhook (webhook) where

import BasicPrelude (liftIO)

import Network.Wai (Application, queryString)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (string, noStoreFileUploads)
import Network.HTTP.Types.QueryLike (toQuery)
import Network.Wai.Parse (parseRequestBody)

import Records (Action)

webhook :: Action Application
webhook _ _ _ _ req = do
	(query, _) <- parseRequestBody noStoreFileUploads req
	liftIO $ appendFile "./webook.log" $
		show (toQuery query) ++ "\n" ++
		show (toQuery $ queryString req) ++ "\n----\n"
	string ok200 [] "OK\n"
