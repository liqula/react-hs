
import Control.Monad.Identity
import System.Mem.StableName
import System.Environment
import System.IO.Unsafe

{-# INLINE mk #-}
mk :: a -> StableName a
mk = unsafePerformIO . makeStableName

{-# NOINLINE f #-}
f = fmap runIdentity . pure . Identity

{-# NOINLINE myid #-}
myid x = x

data X a = X a
instance Eq (X a) where
  X a == X b = mk a == mk b

main :: IO ()
main = do
  [a_] <- getArgs
  let a = a_ ++ "x"
  s <- makeStableName a
  -- gc?
  s' <- makeStableName a
  s'' <- makeStableName (myid a)
  let ia = myid a
  s''' <- makeStableName ia
  let b = a
  a1 <- pure a
  a2 <- pure a
  b1 <- f a
  b2 <- f a
  c1 <- f $ X a
  c2 <- f $ X a
  s'''' <- a `seq` makeStableName a
  s''''' <- ia `seq` makeStableName ia
-- ghc version: 8.2.1
-- ghcjs version: 0.2.1.9008011 (GHC 8.0.2)
--                                       ghci   ghc/-O0
--                                                     ghc -O/-O1/-O2
--                                                            ghcjs/-O0
--                                                                   ghcjs -O/-O1/-O2
  print ( eqStableName s s'           -- True   True   True   True   True
        , eqStableName s s''          -- False  False  False  False  False
        , eqStableName (mk a) (mk a)  -- True   True   True   True   True
        , eqStableName (mk a) (mk b)  -- False  True   True   True   True
        , eqStableName (mk a1) (mk a2)-- True   True   True   True   True
        , eqStableName (mk b1) (mk b2)-- False  False  True   False  True
        , c1 == c2                    -- True   True   True   True   True
        , eqStableName s s''''        -- False  False  False  False  False
        , eqStableName s''''' s'''    -- False  False  False  False  False
        , eqStableName s''''' s'''')  -- True   True   True   False  False
  

