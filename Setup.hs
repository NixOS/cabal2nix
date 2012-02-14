module Main ( main ) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo ( withPrograms )
import Distribution.Simple.Program ( userSpecifyArgs )

main :: IO ()
main = defaultMainWithHooks $
         simpleUserHooks `modify_haddockHook` \oldHH pkg lbi hooks flags ->
           (\lbi' -> oldHH pkg lbi' hooks flags) $
             lbi `modify_withPrograms` \oldWP ->
               userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] oldWP
  where
    modify_haddockHook  hooks f = hooks { haddockHook  = f (haddockHook  hooks) }
    modify_withPrograms lbi   f = lbi   { withPrograms = f (withPrograms lbi)   }
