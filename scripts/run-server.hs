#!/usr/bin/env stack
{- stack --resolver lts-8.11 --install-ghc runghc --package wai --package wai-app-static --package warp -}

import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import System.FilePath (addTrailingPathSeparator)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--warm-up"] -> pure ()  -- (just get the script dependencies compiled so next time it starts faster.)
    [port, path] -> run (read port) . staticApp . defaultFileServerSettings . addTrailingPathSeparator $ path
