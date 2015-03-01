module Main where
import qualified Data.ByteString.Lazy              as LBS
import           System.Directory                  (createDirectoryIfMissing)
import           System.Environment
import           System.FilePath                   (takeExtension)
import           Text.Pandoc
import           Text.Pandoc.MediaBag              (extractMediaBag)
import           Text.Pandoc.Readers.Docx.Japanese

main :: IO ()
main = do
  [fp] <- getArgs
  let ext = tail $ takeExtension fp
      Right reader | ext == "docx" = Right $ ByteStringReader $ (return .) . readDocxJP
                   | otherwise = getReader ext
  putStrLn . writeJSON def =<< feedReader reader def fp

feedReader :: Reader -> ReaderOptions -> FilePath -> IO Pandoc
feedReader (ByteStringReader r) opts fp = do
  (pan, m) <- (r opts =<< LBS.readFile fp)
  createDirectoryIfMissing True "media"
  extractMediaBag True "." m
  return pan
feedReader (StringReader r) opts fp = r opts =<< readFile fp
