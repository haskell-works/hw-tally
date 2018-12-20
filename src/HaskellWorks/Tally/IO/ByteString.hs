module HaskellWorks.Tally.IO.ByteString where

import Control.Monad
import Control.Monad.IO.Class
import Data.List

import qualified Data.ByteString  as BS
import qualified System.Directory as IO

readFilesInDir :: FilePath -> (FilePath -> Bool) -> IO [(FilePath, BS.ByteString)]
readFilesInDir filePath predicate = do
  files <- liftIO $ IO.listDirectory filePath
  let qualifiedFiles = (\f -> filePath <> "/" <> f) <$> filter predicate files
  forM qualifiedFiles $ \qualifiedFile -> do
    contents <- BS.readFile qualifiedFile
    return (qualifiedFile, contents)
