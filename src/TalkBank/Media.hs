{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TalkBank.Media (chatMediaRelativePaths) where

import Data.Monoid ((<>))
import Control.Arrow ((>>>))
import qualified Data.Maybe as Maybe

import Text.Regex.PCRE.Heavy
import Data.Stringable (Stringable)
import Data.String.Conversions (SBS, convertString)

import TalkBank.Re (myRe)

-- | Get all relevant relative paths to external media resources.
chatMediaRelativePaths :: SBS -- ^ CHAT text
                       -> [FilePath]
chatMediaRelativePaths text =
  Maybe.maybeToList (maybeMedia text) ++ getAllPics text

-- | Get one path from Media header if any.
maybeMedia :: SBS -- ^ CHAT text
           -> Maybe FilePath
maybeMedia text =
  maybeFirstMatch mediaRegex text >>= maybeExtractMedia

-- | Get all the pic paths if any.
getAllPics :: SBS -- ^ CHAT text
           -> [FilePath]
getAllPics = scan picRegex >>> map extractPic

-- | Match a media name, audio/video, and optional missing/unlinked.
mediaRegex :: Regex
mediaRegex = [myRe|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]

-- | Ignore anything marked missing/unlinked.
maybeExtractMedia :: (SBS, [SBS])
                  -> Maybe FilePath
maybeExtractMedia (_, [name, "video"]) =
  Just $ convertString (name <> ".mp4")
maybeExtractMedia (_, [name, "audio"]) =
  Just $ convertString (name <> ".mp3")
maybeExtractMedia (_, _) = Nothing

-- | Match a media path.
picRegex :: Regex
picRegex = [myRe|%pic:"([^\"]+)"|]

-- | Get the known single match.
extractPic :: (SBS, [SBS])
           -> FilePath
extractPic (_, [name]) = convertString name
extractPic _ = error "impossible"

-- | Utility for getting just first match if any.
maybeFirstMatch :: Regex -> SBS -> Maybe (SBS, [SBS])
maybeFirstMatch regex = scan regex >>> Maybe.listToMaybe
