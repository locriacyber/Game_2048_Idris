module IORng

import Data.Bits
import Control.Eff
import Control.Eff.Legacy.State
import Effects
import Data.IORef

nextseed : Int -> Int
nextseed seed = 1664525 * seed + 1013904223

nextNumber : Has (State Int) fs => Random Int a -> Eff fs a
nextNumber RandomOne = do
   seed <- get
   let r = abs (seed `shiftR` 2)
   modify nextseed
   pure r

export
handleRandom : Has (Random Int) fs => (seed: Int) -> Eff fs a -> Eff (fs - Random Int) a
handleRandom seed fcont {fs} = do
   let fcont' = lift1 {f=State Int} fcont
   let fcont'' = handleLinear {f=Random Int} nextNumber fcont'
   (ret, _) <- runState seed fcont''
   pure ret
