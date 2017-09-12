{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM, liftM)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Proxy (Proxy(..))
import Data.Text (pack, unpack)
import Data.Vector (toList)
import GitHub.Data (Repo(..))
import GitHub.Data.Definitions (Owner(..))
import GitHub.Data.Name (mkName, untagName)
import GitHub.Data.Repos (RepoPublicity(..))
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.Repos (userRepos)
import Prelude
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Process (system)

import qualified Options.Applicative as O
import qualified Options.Applicative.Builder.Internal as X

toString_ = unpack . untagName

data Options = Options
  { directory :: String
  , user :: String
  } deriving (Show)

main :: IO ()
main = O.execParser (O.info (O.helper <*> options) mempty) >>= start

options :: O.Parser Options
options =
  Options <$>
  defStr
    "."
    (O.metavar "DIRECTORY" <> O.help "Directory to clone everything into") <*>
  O.strOption
    (O.short 'u' <> O.long "user" <>
     O.help "Name of the user to clone the repos of.")

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str

def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional

start :: Options -> IO ()
start opts = do
  putStrLn $ "Looking up repositories for " ++ (user opts) ++ " ..."
  d <-
    fmap (fmap toList) $
    userRepos
      (mkName (Proxy :: Proxy Owner) $ pack $ user opts)
      RepoPublicityAll
  case d of
    Left err -> putStrLn $ show err
    Right repos -> cloneRepos repos opts

cloneRepos :: [Repo] -> Options -> IO ()
cloneRepos rawRepos opts = do
  putStrLn $
    "Found " ++
    (show $ length rawRepos) ++ " repositories for " ++ (user opts) ++ "."
  -- Filter the ones that already exist
  repos <-
    filterM
      (\r ->
         (liftM not) $
         doesDirectoryExist $
         ((directory opts) ++ "/" ++ (toString_ $ repoName r)))
      rawRepos
  putStrLn $ (show $ length repos) ++ " that you don't already have."
  returnCodes <-
    mapM
      (\r ->
         system $
         "cd " ++
         (directory opts) ++
         " && git clone " ++ (unpack $ getUrl $ fromJust $ repoSshUrl r))
      repos
  let rs = all (\e -> e == ExitSuccess) returnCodes
  case rs of
    True -> exitSuccess
    False -> exitFailure
