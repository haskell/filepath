{-# LANGUAGE CPP #-}

#undef  POSIX
#define IS_WINDOWS True
#define WINDOWS
#define FILEPATH_NAME WindowsPath
#define OSSTRING_NAME WindowsString
#define WORD_NAME WindowsChar

#include "Common.hs"


-- | QuasiQuote a 'WindowsPath'. This accepts Unicode characters
-- and encodes as UTF-16LE. Runs 'isValid' on the input.
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF16le ErrorOnCodingFailure) s
      when (not $ isValid ps) $ fail ("filepath not valid: " ++ show ps)
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
