
{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2007
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

A library for 'FilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

Both "System.FilePath.Posix" and "System.FilePath.Windows" provide the
same interface. See either for examples and a list of the available
functions.
-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define FILEPATH_OS Windows
#else
#define FILEPATH_OS Posix
#endif


module System.FilePath(module System.FilePath.FILEPATH_OS) where

import System.FilePath.FILEPATH_OS
