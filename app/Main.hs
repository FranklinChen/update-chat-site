module Main where

import Development.Shake hiding ((*>))

import Control.Arrow ((>>>))
import Control.Monad (when)
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
defaultRootDir hostName = error $ "Unknown host " ++ hostName

main :: IO ()
main = do
  -- Check all the dirs first.
  rootDir <- defaultRootDir <$> HostName.getHostName
  rootDirExists <- Directory.doesDirectoryExist rootDir
  when (not rootDirExists) $
    fail $ "root dir " ++ rootDir ++ " does not exist"

  let dataOrigDir = rootDir </> "data-orig"
  dataOrigDirExists <- liftIO $ Directory.doesDirectoryExist dataOrigDir
  when (not dataOrigDirExists) $
    fail $ "data-orig dir " ++ dataOrigDir ++ " does not exist"

  let mediaDir = rootDir </> "media"
  mediaDirExists <- liftIO $ Directory.doesDirectoryExist mediaDir
  when (not mediaDirExists) $
    fail $ "media dir " ++ mediaDir ++ " does not exist"

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

reportChatFileDeps :: FilePath -- ^ data-orig
                   -> FilePath -- ^ media
                   -> FilePath -- ^ CHAT path relative to root
                   -> IO ()
reportChatFileDeps dataOrigDir mediaDir relativeChatPath =
  requiredMediaPaths chatPath chatMediaDir
  >>= mapM_ (reportExistence chatPath) where
    chatPath = dataOrigDir </> relativeChatPath
    chatMediaDir = mediaDir </> takeDirectory relativeChatPath

reportExistence :: FilePath -- ^ CHAT path
                -> FilePath -- ^ an external Depp path
                -> IO ()
reportExistence chatPath depPath = do
  depExists <- Directory.doesFileExist depPath
  when (not depExists) $
    putStrLn $ chatPath ++ ": cannot find " ++ depPath

-- | Required media files corresponding to a CHAT file.
--
-- Slurp in whole file as 'ByteString' since regex uses that.
requiredMediaPaths :: FilePath -- ^ CHAT path
                   -> FilePath -- ^ CHAT media dir for this file
                   -> IO [FilePath]
requiredMediaPaths chatPath chatMediaDir =
  (Media.chatMediaRelativePaths >>> map (chatMediaDir </>))
  <$> ByteString.readFile chatPath
