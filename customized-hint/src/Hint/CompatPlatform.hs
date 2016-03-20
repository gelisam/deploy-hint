#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
module Hint.CompatPlatform (
    getPID
) where

import Control.Applicative
import Prelude

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

import Data.Word

#else

import System.Posix.Process

#endif

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- This function is not yet in the win32 package, so we have to
-- roll down our own definition.
--
-- Credit goes where it is deserved:
--     http://www.haskell.org/pipermail/haskell-cafe/2009-February/055097.html
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO Word32

getPID :: IO Int
getPID = fromIntegral <$> c_GetCurrentProcessId

#else

getPID :: IO Int
getPID = fromIntegral <$> getProcessID

#endif
