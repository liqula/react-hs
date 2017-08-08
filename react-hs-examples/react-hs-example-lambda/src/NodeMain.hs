import React.Flux
import LambdaViews
import LambdaStore
import qualified Data.Text.IO as T

main :: IO ()
main = do
  registerInitialStore $ initialLambdaState
  reactRenderViewToString True lambdaApp >>= T.putStrLn
