{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSChar8
import           Data.Char                 (ord)
import           Data.Monoid               ((<>))
import           Data.Text                 (unpack)
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Options.Applicative
import           Zelinf.TcpClient.Option

main :: IO ()
main = do
  withSocketsDo (pure ())

  options <- execParser parserInfo
  input <- BS.getContents
  sock <- socket AF_INET Stream defaultProtocol
  hostIP <- inet_addr . unpack . serverIP $ options

  connect sock (SockAddrInet (fromIntegral $ serverPort options) hostIP)

  sendAll sock (convertToCRLF input)
  response <- recvAll sock

  shutdown sock ShutdownBoth
  close sock

  BSChar8.putStrLn response

parserInfo :: ParserInfo Option
parserInfo =
  info (optionParser <**> helper)
    ( fullDesc
      <> progDesc "Send data from stdin to IPV4_ADDR:PORT"
    )

recvAll :: Socket -> IO ByteString
recvAll sock = do
  fmap mconcat
    . takeWhileM (not . BS.null)
    . (iterate step)
    $ recvOnce
  where
  (blockSize :: Int) = 4096
  recvOnce = recv sock blockSize
  step resp = do
    resp' <- resp
    if BS.length resp' == blockSize
      then recvOnce
      else pure ""

convertToCRLF :: ByteString -> ByteString
convertToCRLF = BS.concatMap convertSingle
  where
  convertSingle ch =
    if ch == (fromIntegral . ord $ '\n')
      then "\r\n"
      else BS.pack [ch]

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = pure []
takeWhileM predicate (x:xs) = do
  x' <- x
  if predicate x'
    then (x':) <$> takeWhileM predicate xs
    else pure []
