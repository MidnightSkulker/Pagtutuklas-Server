module GetRepoRoot ( getRepoRoot ) where

import System.FilePath ( combine, takeDirectory )
import System.Directory ( getCurrentDirectory )
import Language.Haskell.TH
import Data.Time.Format ()

-- Return the root of the current repository
getRepoRoot :: Q FilePath
getRepoRoot =
     do here <- location
        cwd <- runIO getCurrentDirectory
        let thisFileName = combine cwd $ loc_filename here
        -- XXX: This depends on the location of this file in the source tree
        return $ head $ drop 2 $ iterate takeDirectory thisFileName
