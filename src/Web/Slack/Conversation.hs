{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Conversation
-- Description:
--
----------------------------------------------------------------------

module Web.Slack.Conversation
  ( Conversation(..)
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Common
import Web.Slack.Util

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Conversation =
  Conversation
    { conversationId :: Text
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversation") ''Conversation)

data ListRsp =
  ListRsp
    { listRspChannels :: [Conversation]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
