{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Lens
import Data.Aeson (FromJSON, toJSON)
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.Wreq
import System.Environment
import Text.Printf

newtype Owner = Owner
  {login :: String}
  deriving (Show, Generic)

data Repo = Repo
  { name :: String,
    full_name :: String,
    owner :: Owner,
    url :: String,
    git_url :: String,
    stargazers_count :: Int,
    watchers_count :: Int,
    language :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Owner

instance FromJSON Repo

type Repos = [Repo]

type PageOfRepos = [Repos]

userData, userRepos, userStarred :: String
userData = "https://github.com/%s/%s"
userRepos = "https://api.github.com/users/%s/repos?page=%d"
userStarred = "https://api.github.com/user/starred/%s"

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

packToken :: String -> B.ByteString
packToken token = packStr'' (printf "token %s" token :: String)

getUserReposFromPage :: String -> Int -> String -> IO Repos
getUserReposFromPage user page token = do
  r <-
    asJSON
      =<< getWith
        ( defaults
            & header "Authorization" .~ [packToken token]
            & header "User-Agent" .~ ["terminal"]
        )
        (printf userRepos user page :: String)

  return $ r ^. responseBody

getAllReposFromUser :: String -> Int -> String -> IO Repos
getAllReposFromUser _ 31 _ = return []
getAllReposFromUser user n token = do
  repos <- getUserReposFromPage user n token
  nextPage <- getUserReposFromPage user (n + 1) token
  return $ repos ++ nextPage

starRepo :: String -> String -> IO ()
starRepo token name = do
  putStrLn $ "Starring: " ++ name

  r <-
    putWith
      ( defaults
          & header "Accept" .~ ["application/vnd.github.v3+json"]
          & header "Authorization" .~ [packToken token]
          & header "User-Agent" .~ ["terminal"]
      )
      (printf userStarred name :: String)
      (packStr'' "")

  return ()

star :: Repos -> String -> IO ()
star pages token = mapM_ (forkIO . starRepo token . full_name) pages

trimNewLine :: String -> String
trimNewLine = reverse . dropWhile (== '\n') . reverse

main :: IO ()
main = do
  user <- getArgs
  token <- readFile . printf "%s/.staritrc" =<< getEnv "HOME"
  repos <- getAllReposFromUser (head user) 1 (trimNewLine token)
  star repos (trimNewLine token)
