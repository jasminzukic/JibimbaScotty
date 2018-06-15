{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import System.Random
import Data.List
import Network.Wai.Middleware.RequestLogger -- TODO v
import Network.Wai.Middleware.Static
import Web.Scotty hiding (delete)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev -- TODO treba samo za development
  middleware $ staticPolicy (noDots >-> addBase "frontend")

  get "/" $ file "index.html"

  get "/syns/:num" $ do
    n <- param "num"
    syns <- liftIO $ generateSyns n
    json syns

  post "/postSyns/:xs" $ do
    xs <- param "xs"
    liftIO $ processWordsResults xs
    json ["back" :: String] -- TODO možda ne treba

processWordsResults :: String -> IO ()
processWordsResults xs = do
  let ys = pairUp $ words xs
  mapM_ (\x -> appendFile "./Korisnicka vremena.txt" $ x ++ "\n") ys
  return ()

pairUp :: [String] -> [String]
pairUp []         = []
pairUp [x]        = []
pairUp (x:[y])    = []
pairUp (x:y:z:zs) = unwords (x:y:[z]) : pairUp zs

generateSyns :: Int -> IO [String]
generateSyns n = do
  allNouns <- readLines (dataFolder ++ "nouns.txt")
  nouns <- sample n allNouns
  commonSyns <- readLines (dataFolder ++ "commonSyns.txt")
  syns <- addAttributes nouns commonSyns
  return syns

dataFolder :: String
dataFolder = "data/"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

addAttributes :: [String] -> [String] -> IO [String]
addAttributes xs commonSyns = mapM (newAttribute commonSyns) xs

newAttribute :: [String] -> String -> IO String
newAttribute commonSyns x = do
  attr <- sample1 =<< getAttributes x
  let syn = head (words attr) ++ " " ++ exactWord x
  if syn `elem` commonSyns
    then newAttribute commonSyns x
    else return syn
  where
    wordType x = (words x) !! 1
    exactWord x = head $ words x

getAttributes :: String -> IO [String]
getAttributes x
  | wordType x == "np" = do
    fsAttrs <- readLines (dataFolder ++ "fsnAttributes.txt")
    npAttrs <- readLines (dataFolder ++ "npnAttributes.txt")
    return $ npAttrs ++ fsAttrs
  | wordType x == "ms" = do
    msAttrs <- readLines (dataFolder ++ "msnAttributes.txt")
    msmpAttrs <- readLines (dataFolder ++ "msmpAttributes.txt")
    return $ msmpAttrs ++ msAttrs
  | wordType x == "mp" = do
    mpnAttrs <- readLines (dataFolder ++ "mpnAttributes.txt")
    msmpAttrs <- readLines (dataFolder ++ "msmpAttributes.txt")
    return $ mpnAttrs ++ msmpAttrs
  | otherwise = do
    attrs <- readLines (dataFolder ++ wordType x ++ "nAttributes.txt")
    return attrs
  where
    wordType x = (words x) !! 1

sample1 :: [a] -> IO a
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx

sample :: Eq a => Int -> [a] -> IO [a]
sample 0 xs = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> (pure val) <*> (sample (l-1) (delete val xs))
