{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

#undef  WINDOWS
#define POSIX
#define IS_WINDOWS False
#define FILEPATH_NAME PosixPath
#define OSSTRING_NAME PosixString
#define WORD_NAME PosixChar

#include "Common.hs"

#if defined(MIN_VERSION_template_haskell) || defined(MIN_VERSION_template_haskell_quasiquoter)
-- | QuasiQuote a 'PosixPath'. This accepts Unicode characters
-- and encodes as UTF-8. Runs 'isValid' on the input.
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF8 ErrorOnCodingFailure) s
      when (not $ isValid ps) $ fail ("filepath not valid: " ++ show ps)
      lift ps
  , quotePat = \s -> do
      osp' <- either (fail . show) pure . encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      when (not $ isValid osp') $ fail ("filepath not valid: " ++ show osp')
      [p|((==) osp' -> True)|]
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a declaration)"
  }
#else
pstr :: a
pstr = error "System.OsPath.Posix.pstr: no Template Haskell"
#endif /* defined(MIN_VERSION_template_haskell) || defined(MIN_VERSION_template_haskell_quasiquoter) */

