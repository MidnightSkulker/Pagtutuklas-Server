{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS -XScopedTypeVariables #-}
module Version
       ( buildString
       , versionString
       ) where

#ifdef PROFILE

versionString, buildString :: String

versionString = "Profiling build"
buildString = "Profiling build"

#else

import BuildInfo ( getBuildDepends, getBuildInfoString )
import Language.Haskell.TH.Syntax (lift)
import GetRepoRoot ( getRepoRoot )

versionString, buildString :: String

versionString = $(getBuildInfoString getRepoRoot >>= lift)
buildString = $(getBuildDepends getRepoRoot >>= lift)

#endif
