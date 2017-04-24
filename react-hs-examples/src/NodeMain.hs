import React.Flux
import TodoViews
import TodoStore
import qualified Data.Text.IO as T

main :: IO ()
main = do
  registerInitialStore $ initialTodos
  reactRenderViewToString True todoApp >>= T.putStrLn
