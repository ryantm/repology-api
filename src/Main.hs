{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Category ((>>>))
import           Control.Error
import           Control.Monad
import           Data.HashMap.Strict
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Network.HTTP.Client.TLS (newTlsManager)
import           Repology
import           Servant.Client (ClientM, ClientEnv (ClientEnv), runClientM)

nixRepo = "nix_unstable"

nixOutdated :: ClientM Metapackages
nixOutdated = metapackages
  Nothing
  Nothing
  Nothing
  (Just nixRepo)
  (Just True)
  Nothing
  Nothing
  Nothing

nextNixOutdated :: Text -> ClientM Metapackages
nextNixOutdated n = metapackages'
  n
  Nothing
  Nothing
  Nothing
  (Just nixRepo)
  (Just True)
  Nothing
  Nothing
  Nothing

outdatedForRepo :: Text -> Vector Package -> Maybe Package
outdatedForRepo r =
  V.find (\p ->
             (status p) == Just "outdated" &&
             (repo p) == r)

newest :: Vector Package -> Maybe Package
newest = V.find (\p -> (status p) == Just "newest")

dropMaybes :: [(Maybe Package, Maybe Package)] -> [(Package, Package)]
dropMaybes = Data.List.foldl' twoJusts []
  where twoJusts a (Just o, Just n) = (o,n):a
        twoJusts a _ = a

getUpdateInfo :: ClientM (Maybe Text, Vector (Package, Package))
getUpdateInfo = do
  outdated <- nixOutdated
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
  return (lastMetapackageName outdated, V.fromList nixNew)

getNextUpdateInfo :: Text -> ClientM (Maybe Text, Vector (Package, Package))
getNextUpdateInfo n = do
  outdated <- nextNixOutdated n
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
  return (lastMetapackageName outdated, V.fromList nixNew)


updateInfo :: (Package, Package) -> Text
updateInfo (outdated, newest) =
  name outdated <>
  " " <>
  version outdated <>
  " " <>
  version newest

moreWork :: (Maybe Text, Vector (Package, Package)) -> Bool
moreWork (Nothing, _) = False
moreWork (_, v) = length v /= 1


moreNixUpdateInfo ::
  (Maybe Text, Vector (Package, Package)) ->
  ClientM (Vector (Package, Package))
moreNixUpdateInfo (Nothing, acc) = do
  result <- getUpdateInfo
  if moreWork result
    then moreNixUpdateInfo (fst result, (snd result) V.++ acc)
    else return acc
moreNixUpdateInfo (Just name, acc) = do
  result <- getNextUpdateInfo name
  if moreWork result
    then moreNixUpdateInfo (fst result, (snd result) V.++ acc)
    else return acc


allNixUpdateInfo :: ClientM (Vector (Package, Package))
allNixUpdateInfo = moreNixUpdateInfo (Nothing, V.empty)

main :: IO ()
main = do
  manager' <- newTlsManager
  res <- runClientM allNixUpdateInfo (ClientEnv manager' baseUrl Nothing)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ois | V.null ois -> putStrLn "No updates needed"
              | otherwise -> do
                  (fmap updateInfo >>>
                    fmap Data.Text.IO.putStrLn >>>
                    V.sequence_) ois
