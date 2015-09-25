{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TalkBank.Media
       (
         ExpectedType(..)
       , typeExpected
       , picRelativePaths
       ) where

import Control.Arrow ((>>>))
import qualified Data.Maybe as Maybe

import Text.Regex.PCRE.Heavy
import Data.String.Conversions (SBS, convertString)

import TalkBank.Re (myRe)

data ExpectedType =
    Skip
  | Video FilePath
  | Audio FilePath
    deriving (Eq, Show)

-- | Get info from Media header.
typeExpected :: SBS -- ^ CHAT text
                -> ExpectedType
typeExpected text =
  maybe Skip extractMediaType $ maybeFirstMatch mediaRegex text

-- | Get all the pic paths if any.
picRelativePaths :: SBS -- ^ CHAT text
                 -> [FilePath]
picRelativePaths = scan picRegex >>> map extractPic

-- | Match a media name, audio/video, and optional missing/unlinked.
mediaRegex :: Regex
mediaRegex = [myRe|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]

-- | Ignore anything marked missing/unlinked.
extractMediaType :: (SBS, [SBS])
                 -> ExpectedType
extractMediaType (_, [name, "video"]) = Video $ convertString name
extractMediaType (_, [name, "audio"]) = Audio $ convertString name
extractMediaType (_, _) = Skip

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
