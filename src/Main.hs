{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client.TLS (newTlsManager)
import Repology
import Servant.Client

queries :: ClientM [Package]
queries = do
  m <- metapackage "2bwm"
  return m

main :: IO ()
main = do
  manager' <- newTlsManager
  res <- runClientM queries (ClientEnv manager' (BaseUrl Https "repology.org" 443 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ps -> do
      print ps
