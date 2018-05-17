module Zelinf.TcpClient.Option
  ( Option(..)
  , optionParser
  ) where

import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Data.Word           (Word16)
import           Options.Applicative

data Option = Option
  { serverIP   :: Text
  , serverPort :: Word16
  }

optionParser :: Parser Option
optionParser = Option
  <$> ( fromString <$> strOption
        ( long "ip"
        <> metavar "IPV4_ADDR"
        <> help "IPv4 address of the server"
        )
      )
  <*> option auto
      (long "port"
     <> metavar "PORT"
     <> help "Port number of the server to connect"
      )
