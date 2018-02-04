{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Repology
import Servant.Client


outdated :: ClientM (HashMap Text [Package])
outdated = metapackages
           Nothing
           Nothing
           Nothing
           (Just "nix_unstable")
           (Just True)
           Nothing
           Nothing
           Nothing

-- queries :: ClientM [Package]
-- queries = do
--   m <- outdated
--   return m

main :: IO ()
main = do
  manager' <- newTlsManager
  res <- runClientM outdated (ClientEnv manager' (BaseUrl Https "repology.org" 443 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ps -> do
      sequence_ (fmap print (keys ps))
