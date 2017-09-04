module Handler.Disco.MX
    ( hasGoogleAppsMX
    , CanResolve(..)
    , MXResponse
    ) where

import Control.Arrow ( (>>>) )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Error ( ErrorT, Error )
import Control.Monad.Trace ( NoTraceT )
import Control.Monad.Trans ( lift )
import Data.Char ( toLower )
import Network.Socket ( HostName, HostAddress )

import ADNS ( initResolver, InitFlag(..) )
import ADNS.Base ( Status )
import qualified ADNS.Resolver as Resolver ( resolveMX )

type MXResponse = Either Status [(HostName, HostAddress)]

class CanResolve m where
    resolveMX :: HostName -> m MXResponse

instance CanResolve IO where
    resolveMX host =
        initResolver flags $ \resolver -> Resolver.resolveMX resolver host
            where flags = [NoErrPrint, NoServerWarn]

instance (Monad m, Error e, CanResolve m) => CanResolve (ErrorT e m) where
    resolveMX = lift . resolveMX

instance (Monad m, CanResolve m) => CanResolve (ReaderT r m) where
    resolveMX = lift . resolveMX

instance (Monad m, CanResolve m) => CanResolve (NoTraceT m) where
    resolveMX = lift . resolveMX

-- * Google Apps MX checker

hasGoogleAppsMX :: [(HostName, a)] -> Bool
hasGoogleAppsMX = any (isGoogleAppsMX . fst)

isGoogleAppsMX :: HostName -> Bool
isGoogleAppsMX = normalizeHostName >>> (`elem` googleAppsMxDomains)

stripTrailingDot :: String -> String
stripTrailingDot s = if last s == '.' then init s else s

normalizeHostName :: String -> String
normalizeHostName = stripTrailingDot >>> (map toLower)

googleAppsMxDomains :: [String]
googleAppsMxDomains =
    map normalizeHostName
    [ "aspmx.l.google.com"
    , "alt1.aspmx.l.google.com"
    , "alt2.aspmx.l.google.com"
    , "aspmx2.googlemail.com"
    , "aspmx3.googlemail.com"
    ]
