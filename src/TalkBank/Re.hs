module TalkBank.Re (myRe) where

import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (multiline, utf8)
import Language.Haskell.TH.Quote (QuasiQuoter)

myRe :: QuasiQuoter
myRe = mkRegexQQ [multiline, utf8]
