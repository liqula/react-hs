module Main where

import Test.Hspec
import System.Process
import qualified Spec

main :: IO ()
main = do
    _ <- system "cd ../.. && make"
    hspec Spec.spec
