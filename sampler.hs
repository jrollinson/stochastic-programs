import Stochastic

import System.Random
import qualified Data.Map as Map

result n gen = 
  let
    network = Map.fromList [ (1, SDataStruct "B" [])
                           , (2, SFlip 0.5)
                           , (3, SDataStruct "A" [])
                           , (4, SDataStruct "C" [1,2,3])
                           , (5, SIndex 4 1)
                           ]

    repeatSample 0 gen = []
    repeatSample n gen = 
      let
        (net, gen') = sample network 5 gen
      in
        net:(repeatSample (n-1) gen')
        
    samples = repeatSample n gen 

  in
    concatMap (\x -> show x ++ "\n") samples


main = do
  gen <- getStdGen
  putStr $ result 10 gen
