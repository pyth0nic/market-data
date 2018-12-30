module Network where

import    qualified Network.Socket as Socket
import                      Network.Socket      ( Socket
                                                , SockAddr(SockAddrInet)
                                                , PortNumber
                                                , setSocketOption
                                                )

import                      Data.ByteString     (ByteString)
import                      Conduit             ( ConduitM
                                                , MonadIO
                                                , yield
                                                , liftIO
                                                )

import          Control.Monad.Trans.Resource    ( MonadResource
                                                , allocate
                                                )

import          Network.Socket.ByteString       (recvFrom)

udpSocket :: String -> PortNumber -> IO Socket
udpSocket host port = do
                    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
                    setSocketOption socket Socket.ReusePort 1
                    address <- Socket.inet_addr host
                    Socket.bind socket (SockAddrInet port address)
                    pure socket

sourceIOSocket :: MonadResource m
               => IO Socket                                     
               -> Int
               -> ConduitM i (ByteString, SockAddr) m ()
sourceIOSocket bindAction sz = do
  (_, s) <- allocate bindAction Socket.close
  sourceSocket s sz

sourceSocket :: MonadIO m
             => Socket                                  -- ^ the socket
             -> Int                                     -- ^ maximum length of datagram to receive
             -> ConduitM i (ByteString, SockAddr) m ()  -- ^ streams downstream the received data and sender address
sourceSocket s sz = do
  msg <- liftIO $ recvFrom s sz
  yield msg
  sourceSocket s sz

udpSource :: (MonadResource m) => String                                   
    -> PortNumber                               
    -> Int
    -> ConduitM i (ByteString, SockAddr) m ()
udpSource host port sz =
  sourceIOSocket (udpSocket host port) sz
