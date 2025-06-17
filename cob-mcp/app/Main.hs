{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import System.IO
import System.Environment
import Control.Exception
import System.Process
import Data.Aeson
import Network.MCP.Server
-- import System.IO (hPutStrLn, stderr)
import Network.MCP.Server.StdIO
import Network.MCP.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Cob hiding (try)

-- A simple echo server for demonstration purposes
main :: IO ()
main = do
  [host, token] <- getArgs

  -- Create server with basic capabilities
  let serverInfo = Implementation
        { serverName = "echo-server"
        , serverVersion = "1.0.0"
        }
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Just $ PromptsCapability True
        }

  -- Create server instance
  server <- createServer serverInfo serverCapabilities "You can use CoB actions to serve as a personal assistant to the CoB developers. You should use the provided actions to make their life easier (e.g. by automatically doing what a client requested of them)."

  -- Register resources [...]

  -- Register tools
  let (-=) a b = a .= (b::String)
  let createUserTool = Tool
        { toolName = "cob-create-user"
        , toolDescription = Just "Creates a new CoB user. Mock field is false if changes should apply persistently."
        , toolInputSchema = object
            [ "type" -= "object"
            , "properties" .= object
                [ "username" .= object [ "type" -= "string" ]
                , "name" .= object [ "type" -= "string" ]
                , "email" .= object [ "type" -= "string" ]
                , "contact" .= object [ "type" -= "string" ]
                , "mock"  .= object [ "type" -= "bool" ]
                ]
            , "required" .= [ "username", "name", "email" :: String ]
            ]
        }

  registerTools server [createUserTool]

  -- Register tool call handler
  registerToolCallHandler server $ \request -> do
    let toolname = callToolName request
        args = callToolArguments request

    -- Handle different tools
    result <- case toolname of
      "cob-create-user" -> do
        pass <- readCreateProcess (shell "tr -dc A-Za-z0-9 </dev/urandom | head -c 13; echo") ""
        let toMock = case Map.lookup "mock" args of
              Just (Bool c) -> c
              _             -> True
        muser <- pure $ do
          String username <- Map.lookup "username" args
          String name <- Map.lookup "name" args
          String email <- Map.lookup "email" args
          let contact = case Map.lookup "contact" args of
                         Just (String c) -> Just c
                         _               -> Nothing
          return $
            User (T.unpack username) (Just pass) (T.unpack name) (T.unpack email) (T.unpack <$> contact) Nothing
        case muser of
          Nothing -> return $ Left $ "Failed to construct a User from: " <> T.pack (show args)
          Just user -> do
            let run a s | toMock    = mockCob 150 s a
                        | otherwise = runCob s a

            -- check if successful by catching
            try (withSession host token (run (createUser user))) >>= \case
              Left e  -> return $ Left $ T.pack $ show (e :: SomeException)
              Right i -> return $ Right $ T.pack $ "User created successfully with id " ++ show i ++ " and password " ++ show pass ++ "."

      _ -> return $ Left $ "Unknown tool: " `T.append` toolname
    case result of
      Right ok -> 
        return $ CallToolResult
          [ ToolContent
            { toolContentType = TextualContent
            , toolContentText = Just ok
            }
          ] False
      Left err ->
        return $ CallToolResult
          [ ToolContent
            { toolContentType = TextualContent
            , toolContentText = Just err
            }
          ] True


  runServerWithSTDIO server
