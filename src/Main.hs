{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.HashMap.Strict
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.HTTP.Client.TLS (newTlsManager)
import Repology
import Servant.Client (ClientEnv(ClientEnv), ClientM, runClientM)
import System.IO

nixRepo = "nix_unstable"

nixOutdated :: ClientM Metapackages
nixOutdated =
  metapackages
    Nothing
    Nothing
    Nothing
    (Just nixRepo)
    (Just True)
    Nothing
    Nothing
    Nothing

nextNixOutdated :: Text -> ClientM Metapackages
nextNixOutdated n =
  metapackages'
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
  V.find (\p -> (status p) == Just "outdated" && (repo p) == r)

newest :: Vector Package -> Maybe Package
newest = V.find (\p -> (status p) == Just "newest")

dropMaybes :: [(Maybe Package, Maybe Package)] -> [(Package, Package)]
dropMaybes = Data.List.foldl' twoJusts []
  where
    twoJusts a (Just o, Just n) = (o, n) : a
    twoJusts a _ = a

getUpdateInfo :: ClientM (Maybe Text, Bool, Vector (Package, Package))
getUpdateInfo = do
  outdated <- nixOutdated
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
  let mLastName = lastMetapackageName outdated
  return (mLastName, length ms /= 1, V.fromList nixNew)

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
getNextUpdateInfo ::
     Text -> ClientM (Maybe Text, Bool, Vector (Package, Package))
getNextUpdateInfo n = do
  outdated <- nextNixOutdated n
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
  let mLastName = lastMetapackageName outdated
  liftIO $ hPutStrLn stderr $ show mLastName
  liftIO $ hPutStrLn stderr $ show (length ms)
  return (mLastName, length ms /= 1, V.fromList nixNew)

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
updateInfo :: (Package, Package) -> Text
updateInfo (outdated, newest) =
  name outdated <> " " <> version outdated <> " " <> version newest

moreNixUpdateInfo ::
     (Maybe Text, Vector (Package, Package))
  -> ClientM (Vector (Package, Package))
moreNixUpdateInfo (Nothing, acc) = do
  (mLastName, moreWork, newNix) <- getUpdateInfo
  liftIO $ V.sequence_ $ fmap Data.Text.IO.putStrLn $ fmap updateInfo newNix
  if moreWork
    then moreNixUpdateInfo (mLastName, newNix V.++ acc)
    else return acc
moreNixUpdateInfo (Just name, acc) = do
  (mLastName, moreWork, newNix) <- getNextUpdateInfo name
  liftIO $ V.sequence_ $ fmap Data.Text.IO.putStrLn $ fmap updateInfo newNix
  if moreWork
    then moreNixUpdateInfo (mLastName, newNix V.++ acc)
    else return acc

allNixUpdateInfo :: ClientM (Vector (Package, Package))
allNixUpdateInfo = moreNixUpdateInfo (Nothing, V.empty)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  manager' <- newTlsManager
  runClientM allNixUpdateInfo (ClientEnv manager' baseUrl Nothing)
  return ()
