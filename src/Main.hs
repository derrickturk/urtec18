{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings, LambdaCase #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import System.IO.Error (catchIOError)

data Paper = Paper { sESSIONTITLE :: T.Text
                   , downloadslug :: T.Text
                   , tITLE :: T.Text
                   , sESSIONDAYDATE :: T.Text
                   , prettyday :: T.Text
                   , daynumber :: T.Text
                   , aUTHORS :: T.Text
                   , cONTROLID :: T.Text
                   , nULL :: T.Text
                   , iNSTITUTIONS :: T.Text
                   , row :: Integer
                   } deriving (Generic, Show)

instance FromJSON Paper where
  parseJSON = withObject "Paper" $ \v -> Paper
    <$> v .: "SESSIONTITLE"
    <*> v .: "downloadslug"
    <*> v .: "TITLE"
    <*> v .: "SESSIONDAYDATE"
    <*> v .: "prettyday"
    <*> v .: "daynumber"
    <*> v .: "AUTHORS"
    <*> v .: "CONTROLID"
    <*> v .: "NULL"
    <*> v .: "INSTITUTIONS"
    <*> v .: "row"

instance ToJSON Paper where
  toJSON (Paper {..}) = object
    [ "SESSIONTITLE" .= sESSIONTITLE
    , "downloadslug" .= downloadslug
    , "TITLE" .= tITLE
    , "SESSIONDAYDATE" .= sESSIONDAYDATE
    , "prettyday" .= prettyday
    , "daynumber" .= daynumber
    , "AUTHORS" .= aUTHORS
    , "CONTROLID" .= cONTROLID
    , "NULL" .= nULL
    , "INSTITUTIONS" .= iNSTITUTIONS
    , "row" .= row
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> processURTECDir dir
    _ -> getProgName >>= \prog ->
      hPutStrLn stderr ("Usage: " ++ prog ++ " urtec-dir")
  `catchIOError` print

processURTECDir :: FilePath -> IO ()
processURTECDir dir = do
  let jsonFile = dir </> "support-files" </> "data.json.js"
  createDirectoryIfMissing False (dir </> "renamed")
  B.stripPrefix "jdata = " <$> B.readFile jsonFile >>= \case
    Just json -> do
      case (decode json :: Maybe [Paper]) of
        Just papers -> mapM_ (renamePaper dir) papers
        Nothing -> hPutStrLn stderr
          "Invalid data.json.js file (can't parse papers)."
    Nothing -> hPutStrLn stderr "Invalid data.json.js file (can't find JSON)."

renamePaper :: FilePath -> Paper -> IO ()
renamePaper dir (Paper {..}) = do
  case extractLink downloadslug of
    Just link -> do
      let src = dir </> link
          dst = dir </> "renamed" </>
            T.unpack cONTROLID ++ "_" ++ T.unpack (fixTitle tITLE) <.> "pdf"
      -- renameFile src dst
      -- probably safer to copy...
      copyFile src dst
    Nothing -> hPutStrLn stderr
      ("Invalid link for paper " ++ T.unpack cONTROLID)

extractLink :: T.Text -> Maybe FilePath
extractLink = fmap (T.unpack . T.takeWhile (/= '"'))
  . T.stripPrefix "<a href=\""

naughtyList :: [Char]
naughtyList = "/<>:\"/\\|?*"

fixTitle :: T.Text -> T.Text
fixTitle = T.foldr
  (\c t -> if c `elem` naughtyList then T.cons '_' t else T.cons c t)
  T.empty
