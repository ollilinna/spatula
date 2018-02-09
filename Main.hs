{- 
Spatula 0.0.0

Filters DOM elements with a css selector and extracts selected attribute values from an html file.
-}

module Main (main) where
 
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Tree.NTree.TypeDefs
import Network.Curl
import Network.URI
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.HandsomeSoup
import Text.Regex.Posix
import Text.XML.HXT.Core

main :: IO ()
main = do
  args <- getArgs

  -- Parse options
  let (actions, nonOptions, errors) = getOpt Permute options args 

  -- Thread defaultOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  -- Set dictionary based on arguments
  let Options { optSelector   = selector 
              , optAttribute  = attribute 
              , optPath       = path 
              , optFile       = file 
              , optDownload   = download } = opts 

  -- Read a local file or curl an url
  if file 
    then 
      do 
        html <- readFile path
        selectDoc selector attribute download $ parseHtml html
    else 
      do
        html <- curlGetString path []
        selectDoc selector attribute download $ parseHtml $ snd html


{-
  Options
-}

data Options = Options  { optSelector   :: String
                        , optAttribute  :: String
                        , optPath       :: FilePath
                        , optFile       :: Bool 
                        , optDownload   :: Bool
                        } deriving (Eq, Ord, Show)

-- | Set default values
defaultOptions :: Options 
defaultOptions = Options  { optSelector = "a"
                        , optAttribute  = "href"
                        , optPath       = "http://example.com/"
                        , optFile       = False
                        , optDownload   = False
                        }

options :: [OptDescr (Options -> IO Options) ]
options =
  [Option ['s'] ["selector"]
    (ReqArg
      (\arg opt -> return opt {optSelector = arg})
      "SELECTOR")
      "CSS selector for DOM elements."
  ,Option ['a'] ["attribute"]
    (ReqArg
      (\arg opt -> return opt {optAttribute = arg})
      "ATTR")
      "HTML attribute to output."
  ,Option ['p'] ["path"]
    (ReqArg
      (\arg opt -> return opt {optPath = arg})
      "PATH")
      "URL or file path to HTML to parse."
  ,Option ['f'] ["file"]
    (NoArg
      (\opt -> return opt {optFile = True}))
      "Does the path point to a file on the filesystem?"
  ,Option ['d'] ["download"]
    (NoArg
      (\opt -> return opt {optDownload = True}))
      "Download the result? Works for valid urls."
  ,Option ['h'] ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr (usageInfo prg options)
          exitSuccess))
      "Prints this help message."
  ]

{-
  Utilities
-}
-- | Prints XmlTree to stdout
printDoc :: IOSArrow XmlTree (Data.Tree.NTree.TypeDefs.NTree XNode) -> IO ()
printDoc = (>>= print) . runX . xshow

-- | Filter DOM elements with a selector and print out attributes to stdout or download the contents of an url to disk
selectDoc :: String -> String -> Bool -> IOSArrow XmlTree (Data.Tree.NTree.TypeDefs.NTree XNode) -> IO ()
selectDoc slctr attr dl doc = do 
  x <- runX $ doc >>> css slctr ! attr
  if dl then mapM_ download x else mapM_ print x

-- | Extract filename from an uripath
filename :: String -> String
filename = (=~ "[A-Za-z0-9_-]+\\.[a-z]+$")

-- | Downloads url contents to disk
download :: String -> IO ()
download url = do 
  let name = filename . uriPath . fromJust . parseURI $ url
  callCommand $ "curl " ++ url ++ " > " ++ name
  print $ "Downloaded " ++ name