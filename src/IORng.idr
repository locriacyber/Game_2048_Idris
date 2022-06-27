module IORng

import Data.Bits
import Control.Eff
import Effects
import Data.IORef

nextseed : Int -> Int
nextseed seed = 1664525 * seed + 1013904223

nextNumber : IORef Int -> (forall a . Random Int a -> IO a)
nextNumber seed' RandomOne = do
   seed <- readIORef seed'
   let r = abs (seed `shiftR` 2)
   modifyIORef seed' nextseed
   pure r


export
createRng : (seed: Int) -> IO (forall a . Random Int a -> IO a)
createRng seed = do
   seed' <- newIORef $ nextseed seed
   let -- workaround idris2 bug
      erased0 : IO (forall a . Random Int a -> IO a)
      erased0 = pure {f=IO} $ nextNumber seed'
   erased0
