{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack
-- Description:
--
----------------------------------------------------------------------

module Web.Slack
  -- Config
  ( SlackConfig(..)
  , mkSlackConfig
  -- Test
  , apiTest
  , authTest
  -- Methods
  , chatPostMessage
  , historyFetchAll
  , getUserDesc
  , usersList
  , usersConversations
  , userLookupByEmail
  , authenticateReq
  , Response
  -- Aux
  , HasManager(..)
  , HasToken(..)
  )
  where

-- aeson
import Data.Aeson

-- base
import Control.Arrow ((&&&))
import Data.Maybe
import Data.Proxy (Proxy(..))

-- containers
import qualified Data.Map as Map

-- error
import Control.Error (lastZ, isNothing)

-- http-client
import Network.HTTP.Client (Manager, newManager)

-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- mtl
import Control.Monad.Reader

-- servant
import Servant.API

-- servant-client
import Servant.Client hiding (Response, baseUrl)
import Servant.Client.Core (Request, appendToQueryString)

-- slack-web
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Auth as Auth
import qualified Web.Slack.Chat as Chat
import qualified Web.Slack.Common as Common
import qualified Web.Slack.User as User
import qualified Web.Slack.Conversation as Conversation

-- text
import Data.Text (Text)

#if !MIN_VERSION_servant(0,13,0)
mkClientEnv :: Manager -> BaseUrl -> ClientEnv
mkClientEnv = ClientEnv
#endif

#if MIN_VERSION_servant(0,16,0)
import Servant.Client.Core (AuthenticatedRequest, AuthClientData, mkAuthenticatedRequest, ClientError)
#else
import Servant.Client.Core.Internal.Auth
import Servant.Client.Core (ServantError)
type ClientError = ServantError
#endif

class HasManager a where
    getManager :: a -> Manager

class HasToken a where
    getToken :: a -> Text

-- | Implements the 'HasManager' and 'HasToken' typeclasses.
data SlackConfig
  = SlackConfig
  { slackConfigManager :: Manager
  , slackConfigToken :: Text
  }

instance HasManager SlackConfig where
    getManager = slackConfigManager
instance HasToken SlackConfig where
    getToken = slackConfigToken

-- contains errors that can be returned by the slack API.
-- contrast with 'SlackClientError' which additionally
-- contains errors which occurred during the network communication.
data ResponseSlackError = ResponseSlackError Text
  deriving (Eq, Show)

type Response a =  Either Common.SlackClientError a

-- |
-- Internal type!
--
newtype ResponseJSON a = ResponseJSON (Either ResponseSlackError a)

instance FromJSON a => FromJSON (ResponseJSON a) where
    parseJSON = withObject "Response" $ \o -> do
        ok <- o .: "ok"
        ResponseJSON <$> if ok
           then Right <$> parseJSON (Object o)
           else Left . ResponseSlackError <$> o .: "error"


-- |
--
--

type Api =
    "api.test"
      :> ReqBody '[FormUrlEncoded] Api.TestReq
      :> Post '[JSON] (ResponseJSON Api.TestRsp)
  :<|>
    "auth.test"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Auth.TestRsp)
  :<|>
    "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] (ResponseJSON Chat.PostMsgRsp)
  :<|>
    "users.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON User.ListRsp)
  :<|>
    "users.conversations"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.ListReq
      :> Post '[JSON] (ResponseJSON Conversation.ListRsp)
  :<|>
    "users.lookupByEmail"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] User.Email
      :> Post '[JSON] (ResponseJSON User.UserRsp)


-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>

apiTest
  :: (MonadReader env m, HasManager env, MonadIO m)
  => Api.TestReq
  -> m (Response Api.TestRsp)
apiTest req = run (apiTest_ req)

apiTest_
  :: Api.TestReq
  -> ClientM (ResponseJSON Api.TestRsp)

-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>

authTest
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response Auth.TestRsp)
authTest = do
  authR <- mkSlackAuthenticateReq
  run (authTest_ authR)

authTest_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON Auth.TestRsp)

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>

chatPostMessage
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Chat.PostMsgReq
  -> m (Response Chat.PostMsgRsp)
chatPostMessage postReq = do
  authR <- mkSlackAuthenticateReq
  run (chatPostMessage_ authR postReq)

chatPostMessage_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Chat.PostMsgReq
  -> ClientM (ResponseJSON Chat.PostMsgRsp)

-- |
--
-- <https://api.slack.com/methods/users.conversations>

usersConversations
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Conversation.ListReq
  -> m (Response Conversation.ListRsp)
usersConversations listReq = do
  authR <- mkSlackAuthenticateReq
  run (usersConversations_ authR listReq)

usersConversations_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Conversation.ListReq
  -> ClientM (ResponseJSON Conversation.ListRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>

usersList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response User.ListRsp)
usersList = do
  authR <- mkSlackAuthenticateReq
  run (usersList_ authR)

usersList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON User.ListRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.lookupByEmail>

userLookupByEmail
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => User.Email
  -> m (Response User.UserRsp)
userLookupByEmail email = do
  authR <- mkSlackAuthenticateReq
  run (userLookupByEmail_ authR email)

userLookupByEmail_
  :: AuthenticatedRequest (AuthProtect "token")
  -> User.Email
  -> ClientM (ResponseJSON User.UserRsp)


-- | Returns a function to get a username from a 'Common.UserId'.
-- Comes in handy to use 'Web.Slack.MessageParser.messageToHtml'
getUserDesc
  :: (Common.UserId -> Text)
  -- ^ A function to give a default username in case the username is unknown
  -> User.ListRsp
  -- ^ List of users as known by the slack server. See 'usersList'.
  -> (Common.UserId -> Text)
  -- ^ A function from 'Common.UserId' to username.
getUserDesc unknownUserFn users =
  let userMap = Map.fromList $ (User.userId &&& User.userName) <$> User.listRspMembers users
  in
    \userId -> fromMaybe (unknownUserFn userId) $ Map.lookup userId userMap

-- |
-- Fetch all history items between two dates. The basic calls
-- 'channelsHistory', 'groupsHistory', 'imHistory' and so on
-- may not return exhaustive results if there were too many
-- records. You need to use 'Web.Slack.Common.historyRspHasMore' to find out
-- whether you got all the data.
--
-- This function will repeatedly call the underlying history
-- function until all the data is fetched or until a call
-- fails, merging the messages obtained from each call.
historyFetchAll
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => (Common.HistoryReq -> m (Response Common.HistoryRsp))
  -- ^ The request to make. Can be for instance 'mpimHistory', 'channelsHistory'...
  -> Text
  -- ^ The channel name to query
  -> Int
  -- ^ The number of entries to fetch at once.
  -> Common.SlackTimestamp
  -- ^ The oldest timestamp to fetch records from
  -> Common.SlackTimestamp
  -- ^ The newest timestamp to fetch records to
  -> m (Response Common.HistoryRsp)
  -- ^ A list merging all the history records that were fetched
  -- through the individual queries.
historyFetchAll makeReq channel count oldest latest = do
    -- From slack api-doc: If there are more than 100 messages between
    -- the two timestamps then the messages returned are the ones closest to latest.
    -- In most cases an application will want the most recent messages
    -- and will page backward from there.
    --
    -- for reference (does not apply here) => If oldest is provided but not
    -- latest then the messages returned are those closest to oldest,
    -- allowing you to page forward through history if desired.
    rsp <- makeReq $ Common.HistoryReq channel count (Just latest) (Just oldest) False
    case rsp of
      Left _ -> return rsp
      Right (Common.HistoryRsp msgs hasMore) -> do
          let oldestReceived = Common.messageTs <$> lastZ msgs
          if not hasMore || isNothing oldestReceived
              then return rsp
              else mergeResponses msgs <$>
                   historyFetchAll makeReq channel count oldest (fromJust oldestReceived)

mergeResponses
  :: [Common.Message]
  -> Response Common.HistoryRsp
  -> Response Common.HistoryRsp
mergeResponses _ err@(Left _) = err
mergeResponses msgs (Right rsp) =
    Right (rsp { Common.historyRspMessages = msgs ++ Common.historyRspMessages rsp })

-- API Client definition
--
apiTest_
  :<|> authTest_
  :<|> chatPostMessage_
  :<|> usersList_
  :<|> usersConversations_
  :<|> userLookupByEmail_
  =
  client (Proxy :: Proxy Api)


-- |
--
--

type instance AuthClientData (AuthProtect "token") =
  Text


-- |
--
--

authenticateReq
  :: Text
  -> Request
  -> Request
authenticateReq token =
  appendToQueryString "token" (Just token)


-- |
--
--

run
  :: (MonadReader env m, HasManager env, MonadIO m)
  => ClientM (ResponseJSON a)
  -> m (Response a)
run clientAction = do
  env <- ask
  let baseUrl = BaseUrl Https "slack.com" 443 "/api"
  unnestErrors <$> liftIO (runClientM clientAction $ mkClientEnv (getManager env) baseUrl)

mkSlackAuthenticateReq :: (MonadReader env m, HasToken env)
  => m (AuthenticatedRequest (AuthProtect "token"))
mkSlackAuthenticateReq = flip mkAuthenticatedRequest authenticateReq . getToken <$> ask

unnestErrors :: Either ClientError (ResponseJSON a) -> Response a
unnestErrors (Right (ResponseJSON (Right a))) = Right a
unnestErrors (Right (ResponseJSON (Left (ResponseSlackError serv))))
    = Left (Common.SlackError serv)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)


-- | Prepare a SlackConfig from a slack token.
-- You can then call the other functions providing this in a reader context.
--
mkSlackConfig :: Text -> IO SlackConfig
mkSlackConfig token = SlackConfig <$> newManager tlsManagerSettings <*> pure token
