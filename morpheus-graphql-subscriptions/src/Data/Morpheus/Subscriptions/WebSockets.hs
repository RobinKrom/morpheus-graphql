{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscriptions.WebSockets
  ( webSocketsWrapper,
  )
where

import Control.Monad.Trans.Control
  ( MonadBaseControl,
    embed_
  )
import Data.Morpheus.Subscriptions.Internal
  ( ApiContext (..),
    SUB,
    Store (..),
    acceptApolloRequest,
  )
import Network.WebSockets
  ( Connection,
    ServerApp,
    receiveData,
    sendTextData,
  )
import qualified Network.WebSockets as WS
import Relude

defaultWSScope :: MonadIO m => Store e m -> Connection -> ApiContext SUB e m
defaultWSScope Store {writeStore} connection =
  SubContext
    { listener = liftIO (receiveData connection),
      callback = liftIO . sendTextData connection,
      updateStore = writeStore
    }

webSocketsWrapper ::
  (MonadBaseControl IO m, MonadIO m) =>
  Store e m ->
  (ApiContext SUB e m -> m ()) ->
  m ServerApp
webSocketsWrapper store handler =
  embed_ $
    \pending -> do
      conn <- liftIO $ acceptApolloRequest pending
      handler (defaultWSScope store conn)
