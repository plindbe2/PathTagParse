import Control.Applicative ((<$>))
import Control.Monad (filterM, forM_, liftM, mapM, when)
import Data.List (reverse)
import Data.Maybe (fromMaybe)
import ID3.Simple
import ID3.Type.Tag (emptyID3Tag, ID3Tag(..))
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle(..), stderr, hPutStrLn)
import Text.RegexPR (matchRegexPR)

data Flag a
  = Artist a | Title a | Album a 
  | Year a | Track a | Path a | Regex a
    deriving Show

class TaggingFunc f where
    taggingFunc :: f String -> Tag -> Tag


class Ctor f where
    ctor :: f a -> a -> f a

instance TaggingFunc Flag where
    taggingFunc (Artist a) = setArtist a
    taggingFunc (Title a)  = setTitle a
    taggingFunc (Album a)  = setAlbum a
    taggingFunc (Year a)   = setYear a
    taggingFunc (Track a)  = setTrack $ show (read a :: Int) ++ "/0"

instance Ctor Flag where
    ctor (Artist a) = Artist
    ctor (Title a) = Title
    ctor (Album a) = Album
    ctor (Year a) = Year
    ctor (Track a) = Track

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents path = do
    names <- getUsefulContents path >>= \x ->
      return $ map (path </>) x
    dirs <- filterM doesDirectoryExist names
    liftM concat (mapM getRecursiveContents dirs) >>= \x ->
      filterM doesFileExist names >>= \y ->
      return $ x ++ y
  where getUsefulContents path = do
          names <- getDirectoryContents path
          return (filter (`notElem` [".", ".."]) names)

options :: [OptDescr (Flag String)]
options =
  [ Option ['p'] ["path"]   (ReqArg Path "DIR")             "Path"
  , Option ['r'] ["regex"]  (ReqArg Regex "REGEX")          "Regex"
  , Option ['a'] ["artist"] (OptArg (defFlag Artist "") "") "Artist"
  , Option ['t'] ["title"]  (OptArg (defFlag Title "") "")  "Title"
  , Option ['A'] ["album"]  (OptArg (defFlag Album "") "")  "Album"
  , Option ['y'] ["year"]   (OptArg (defFlag Year "") "")   "Year"
  , Option ['T'] ["track"]  (OptArg (defFlag Track "0") "") "Track"
  ]
  where defFlag flag defVal = maybe (flag defVal) flag

getUsage :: String -> String
getUsage progName = "Usage: " ++ progName ++ " [Path] [Regex] [<Artist> <Album> ...]"

getOpts :: String -> [String] -> IO [Flag String]
getOpts progName argv =
  case getOpt Permute options argv of
     (o,_,[]  ) -> return o
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo (getUsage progName) options))

getTagFunction :: [Flag String] -> [(Int, String)] -> Tag -> Tag
getTagFunction flags matches = foldr ((.) . getTaggingFunc) id (reverse matches)
  where getTaggingFunc x = taggingFunc $ uncurry (getFlagObj flags) x
        -- This could use improvement.
        getFlagObj flags n = ctor $ flags!!(n-1)

main :: IO ()
main = do
    progName <- getProgName
    opts <- getOpts progName =<< getArgs
    when (length opts < 2) $ do
      hPutStrLn stderr $ getUsage progName
      exitFailure
    let (Path path:Regex regex:fields) = opts
    files <- getRecursiveContents path
    forM_ files $ \f -> do
      tag <- fromMaybe emptyID3Tag <$> readTag f
      -- These are ugly hidden class types, so that's where why I show and read them.
      maybe (hPutStrLn stderr $ "Match failed with \"" ++ f ++ "\"")
        (\x -> writeTag f $ getTagFunction fields x tag) $ (map ((\x -> read x :: (Int, String)) . show) . snd) <$> matchRegexPR regex f
