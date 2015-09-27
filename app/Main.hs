module Main where

import Development.Shake

import Data.Monoid ((<>))
import Control.Arrow ((>>>))
import Control.Monad.Extra
import System.FilePath ((</>), takeDirectory)
import qualified System.Directory as Directory
import qualified GHC.Conc as Conc
import qualified Data.ByteString as ByteString
import qualified Network.HostName as HostName

import qualified TalkBank.Media as Media

-- | Hidden Shake build directory for this project.
getShakeBuildDir :: IO FilePath
getShakeBuildDir = (</> ".update-chat-site") <$> Directory.getHomeDirectory

-- | Where to find "data-orig" and "media".
defaultRootDir :: HostName.HostName -> FilePath
defaultRootDir "childes.talkbank.org" = "/web/childes"
defaultRootDir "talkbank.talkbank.org" = "/TalkBank"
defaultRootDir "homebank.talkbank.org" = "/HomeBank"
defaultRootDir hostName = error $ "Unknown host " <> hostName

main :: IO ()
main = do
  -- Check all the dirs first.
  rootDir <- defaultRootDir <$> HostName.getHostName
  unlessM (Directory.doesDirectoryExist rootDir) $
    fail $ "root dir " <> rootDir <> " does not exist"

  let dataOrigDir = rootDir </> "data-orig"
  unlessM (liftIO $ Directory.doesDirectoryExist dataOrigDir) $
    fail $ "data-orig dir " <> dataOrigDir <> " does not exist"

  let mediaDir = rootDir </> "media"
  unlessM (liftIO $ Directory.doesDirectoryExist mediaDir) $
    fail $ "media dir " <> mediaDir <> " does not exist"

  buildDir <- getShakeBuildDir
  numProcessors <- Conc.getNumProcessors

  shakeArgs shakeOptions { shakeFiles = buildDir
                         , shakeThreads = numProcessors
                         , shakeProgress = progressSimple
                         } $ do
    action $ checkMediaFiles dataOrigDir mediaDir

-- | For each CHAT file, check as needed for the existence of video or audio
-- if not marked as "missing".
--
-- Print out anything not found.
checkMediaFiles :: FilePath -- ^ data-orig
                -> FilePath -- ^ media
                -> Action ()
checkMediaFiles dataOrigDir mediaDir = do
  getDirectoryFiles dataOrigDir ["//*.cha"]
    >>= (mapM_ (reportChatFileDeps dataOrigDir mediaDir) >>> liftIO)

-- | Report on existence of relevant dependent files (media, pic).
reportChatFileDeps :: FilePath -- ^ data-orig
                   -> FilePath -- ^ media
                   -> FilePath -- ^ CHAT path relative to root
                   -> IO ()
reportChatFileDeps dataOrigDir mediaDir relativeChatPath = do
  let chatPath = dataOrigDir </> relativeChatPath
  let chatMediaDir = mediaDir </> takeDirectory relativeChatPath

  text <- ByteString.readFile chatPath

  checkMedia chatPath chatMediaDir (Media.typeExpected text)
  mapM_ (checkPic chatPath chatMediaDir) (Media.picRelativePaths text)

-- | Report whether a specific dependent file exists.
reportExistence :: FilePath -- ^ CHAT path
                -> FilePath -- ^ an external dep path
                -> IO ()
reportExistence chatPath depPath = do
  unlessM (Directory.doesFileExist depPath) $
    putStrLn $ chatPath <> ": cannot find " <> depPath

videoExtension :: FilePath
videoExtension = ".mp4"

audioExtension :: FilePath
audioExtension = ".mp3"

-- | Check whether what is expected exists.
checkMedia :: FilePath -- ^ CHAT path
           -> FilePath -- ^ CHAT media dir
           -> Media.ExpectedType
           -> IO ()
checkMedia _ _ Media.Skip = pure ()
checkMedia chatPath chatMediaDir (Media.Video name) =
  reportExistence chatPath (chatMediaDir </> name <> videoExtension)
checkMedia chatPath chatMediaDir (Media.Audio name) = do
  reportExistence chatPath (chatMediaDir </> name <> audioExtension)

checkPic :: FilePath -- ^ CHAT path
         -> FilePath -- ^ Chat media dir
         -> FilePath -- ^ pic relative path
         -> IO ()
checkPic chatPath chatMediaDir picRelativePath =
  reportExistence chatPath (chatMediaDir </> picRelativePath)
