module Network.XmlRpc.Introspect where

import Network.XmlRpc.Internals
import Network.XmlRpc.Client
import qualified Data.ByteString.UTF8       as U
import qualified Network.Browser as B
import Network.HTTP

type Signature = ([Type],Type)
type Help = String
type MethodInfo = (String,[Signature],Help)

-- Primitive introspection functions

listMethods :: String -> (B.BrowserAction (HandleStream U.ByteString)) [String]
listMethods url = remote url "system.listMethods"

methodSignature :: String -> String -> (B.BrowserAction (HandleStream U.ByteString)) [[String]]
methodSignature url = remote url "system.methodSignature"

methodHelp :: String -> String -> (B.BrowserAction (HandleStream U.ByteString)) String
methodHelp url = remote url "system.methodHelp"


signatures :: String -> String -> (B.BrowserAction (HandleStream U.ByteString)) [Signature]
signatures url name = do
		      sigs <- methodSignature url name
		      return [ (map read as,read r) | (r:as) <- sigs ]

methodInfo :: String -> String -> (B.BrowserAction (HandleStream U.ByteString)) MethodInfo
methodInfo url name = do
		      sigs <- signatures url name
		      help <- methodHelp url name
		      return (name, sigs, help)
