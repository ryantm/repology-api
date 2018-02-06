{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Category
import           Control.Error
import           Data.HashMap.Lazy
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO
import           Network.HTTP.Client.TLS (newTlsManager)
import           Repology
import           Servant.Client

nixRepo = "nix_unstable"

nixOutdated :: ClientM (HashMap Text [Package])
nixOutdated = metapackages
           Nothing
           Nothing
           Nothing
           (Just nixRepo)
           (Just True)
           Nothing
           Nothing
           Nothing

findForRepo :: Text -> [Package] -> Maybe Package
findForRepo r = find (\ p -> (repo p) == r)

newestPackage :: Text -> ClientM (Maybe Package)
newestPackage pName = do
  ps <- metapackage pName
  pure (find (\p -> (status p) == Just "newest") ps)


type OutdatedInfo = (Package, Package)

outdatedInfo :: Package -> ClientM (Maybe OutdatedInfo)
outdatedInfo p = do
  np <- newestPackage (name p)
  case np of
    Nothing -> return Nothing
    Just np' ->
      return (Just (p, np'))

outdatedInfos :: ClientM [OutdatedInfo]
outdatedInfos = do
  outdated <- nixOutdated
  (elems >>>                           -- [[Package]]
   fmap (findForRepo nixRepo) >>>      -- [Maybe Package]
   catMaybes >>>                       -- [Package]
   fmap outdatedInfo >>>               -- [ClientM (Maybe OutdatedInfo)]
   sequence >>>                        -- ClientM [Maybe OutdatedInfo]
   fmap catMaybes) outdated

updateInfo :: (Package, Package) -> Text
updateInfo (outdated, newest) =
  "- [ ] " <>
  name outdated <>
  " " <>
  version outdated <>
  " (" <>
  repo outdated <>
  ")" <>
  " -> " <>
  version newest <>
  " (" <>
  repo newest <>
  ")"

main :: IO ()
main = do
  manager' <- newTlsManager
  res <- runClientM outdatedInfos (ClientEnv manager' baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right [] -> putStrLn "No updates needed"
    Right ois ->
      (fmap updateInfo >>>
      fmap Data.Text.IO.putStrLn >>>
      sequence_) ois
