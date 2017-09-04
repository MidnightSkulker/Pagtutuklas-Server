module Config.MailConfig
    ( sendMessage
    , mkMailMessage
    , mkMailConfig
    , getMailDomain
    , getMailServerName
    , getMailServerAddr
    , MailConfig(mailDomain, mailServerName)
    , HasMailConfig (..)
    , MailMessage
    , ToAddress
    , FromAddress
    ) where

import Network.SMTP.Client ( sendSMTP, Message(..), Field(..), NameAddr(..) )
import Network.Socket ( getAddrInfo, addrAddress, SockAddr )

type HostName = String
type MailDomain = String

data MailConfig = MailConfig { mailDomain     :: MailDomain
                             , mailServerAddr :: SockAddr
                             , mailServerName :: HostName
                             } deriving ( Show )

-- | Class characterizing having mail configuration
class HasMailConfig c where
  getMailConfig :: c -> MailConfig
  
-- | Fish mail domain out of the global config
getMailDomain :: ( HasMailConfig c ) => c -> MailDomain
getMailDomain c = mailDomain ( getMailConfig c )

-- | Fish mail server name out of the global config
getMailServerName :: ( HasMailConfig c ) => c -> HostName
getMailServerName c = mailServerName ( getMailConfig c )

-- | Fish mail server address out of the global config
getMailServerAddr :: ( HasMailConfig c ) => c -> SockAddr
getMailServerAddr c = mailServerAddr ( getMailConfig c )

type FromAddress = String
type ToAddress = String

data MailMessage = MailMessage { msgFromAddr :: FromAddress -- |The message from-address
                               , msgToAddrs :: [ToAddress] -- |The message to-address list
                               , msgSubject :: String -- |The message subject
                               , msgBody :: String -- |The message body
                               }

mkMailMessage :: FromAddress -> [ToAddress] -> String -> String -> Maybe MailMessage
mkMailMessage _ [] _ _ = Nothing
mkMailMessage from to subj body = Just $ MailMessage from to subj body

mkMailConfig :: HostName -> MailDomain -> IO (Maybe MailConfig)
mkMailConfig serverName domain = do
  lookups <- getAddrInfo Nothing (Just serverName) (Just "smtp")
  case lookups of
    [] -> return Nothing
    (l:_) -> return $ Just $ MailConfig { mailDomain = domain
                                        , mailServerAddr = addrAddress l
                                        , mailServerName = serverName
                                        }

smtpMessage :: MailMessage -> Message
smtpMessage msg = Message fields body
    where
      body = msgBody msg
      fields = [ subject, to, from ]
      to = To [NameAddr Nothing a | a <- msgToAddrs msg]
      from = From [NameAddr Nothing $ msgFromAddr msg]
      subject = Subject $ msgSubject msg

sendMessage :: MailConfig -> MailMessage -> IO ()
sendMessage config m =
  sendSMTP Nothing (mailDomain config) (mailServerAddr config) [smtpMessage m]
