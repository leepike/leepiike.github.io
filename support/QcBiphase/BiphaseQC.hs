{-
Lee Pike <leepike @ galois . com> (remove spaces)
Discrete-event type simulator for the Biphase Mark Protocol.
See http://www.cs.indiana.edu/~lepike/pub_pages/qc-biphase.html and the associated paper.
BSD3 License.
-}

module Main

where

import Biphase
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Gen

-- | Number of rounds to execute
iter :: Integer
iter = 1

-- | Property should always hold for good parameters.
prop_correct :: Bool -> Property
prop_correct output = 
    assertFinal output forallValidParams bitsEq

-- | Testing should fail on this property for some 
-- percentage of tests.
prop_incorrect :: Bool -> Property
prop_incorrect output = 
    assertFinal output forallInvalidParams bitsEq

-- | Did the receiver get the bits sent by the sender upon 
-- synchronizing?
bitsEq :: (Rx, Tx) -> Bool
bitsEq (rx, tx) = (tbit tx) == (rbit rx)

-- | Note: monadicIO (from QuickCheck) uses unsafeperformIO.
assertFinal :: Bool -> ParamGen -> ((Rx, Tx) -> Bool) -> Property
assertFinal output genParams pred = 
    monadicIO $ genParams $ \p -> 
        assert . pred =<< run (startExec output p iter)

----------- SIMPLE MAIN FUNCTION (modify as needed) -------
main = do
  putStrLn ""
  putStrLn $ "Enter the number of bits to encode" 
       ++ " (an integer between 1 and 100 million): "
  s <- getLine
  putStrLn $ "Show output for each test? (True or False)"
  output <- getLine
  let i = read s
    in if i < 1 || i > 10^8
         then main
         else quickCheckQuotientWith stdArgs {maxSuccess = i} (prop_correct $ read output)

-----------------------------------------------------

type ParamGen = 
    (Params -> PropertyM IO ()) -> PropertyM IO ()

-- | Generating correct params is "too hard" to do 
-- procedurally, so we get close and then use a 
-- predicate to make sure we're only testing correct ones.
forallValidParams :: ParamGen 
forallValidParams = 
    forAllM (genParams `suchThat` correctParams)

-- | Generate *almost* correct realtime parameters --- it's an 
-- overapproximation.  We need to test them to ensure 
-- correctness.
genParams :: Gen Params
genParams = do 
  -- arbitrary-sized clock period
  tperiod <- choose (0, 100) 
  -- The remaining generated values are over-approximations.
  tsettle <- choose (0, tperiod) 
  rscanmin <- choose (0, tperiod - tsettle) 
  rscanmax <- choose (rscanmin, tperiod) 
  rsampmin <- choose ( tperiod + tsettle
                     , 2 * tperiod - tsettle - rscanmax)
  rsampmax <- choose ( rsampmin
                     , 2 * tperiod - tsettle - rscanmax) 
  return $ Params tperiod tsettle rscanmin
                  rscanmax rsampmin rsampmax


-- | Constraints are satisfied.  Reproduced for genParams.
correctParams :: Params -> Bool
correctParams p = 
     0 < tPeriod p -- tPeriod
  && 0 <= tSettle p -- tSettle
  && tSettle p < tPeriod p -- tSettle
  && 0 < rScanMin p -- rScanMin
  && rScanMin p <= rScanMax p -- rScanMax
  && rScanMax p < tStable  -- rScanMax
  && tPeriod p + tSettle p < rSampMin p -- rSampMin
  && rSampMin p <= rSampMax p -- rSampMax
  -- rSampMax
  && rSampMax p < tPeriod p + tStable - rScanMax p 
   where tStable = tPeriod p - tSettle p 

--- GENERATING FAILING TESTS ---

forallInvalidParams :: ParamGen 
forallInvalidParams = 
    forAllM (badGenParams `suchThat` incorrectParams)

-- Example in hte paper.
badGenParams :: Gen Params
badGenParams =
    let tperiod = 100
        tsettleNewMax = 15
        tsettle = 5
    in do 
  -- arbitrary-sized clock period
  --  tperiod <- choose (0, 100) 
  -- The remaining generated values are over-approximations.
  tsettle' <- choose (0, tsettleNewMax) 
  rscanmin <- choose (0, tperiod - tsettle) 
  rscanmax <- choose (rscanmin, tperiod) 
  rsampmin <- choose ( tperiod + tsettle
                     , 2 * tperiod - rscanmax - tsettle
                     )
  rsampmax <- choose ( rsampmin
                     , 2 * tperiod - rscanmax - tsettle
                     ) 
  return $ Params tperiod tsettle' rscanmin
                  rscanmax rsampmin rsampmax


-- Constraints are satisfied.  Reproduced for genParams.
incorrectParams :: Params -> Bool
incorrectParams p = 
    let tSettle' = 5
    in    0 < tPeriod p -- tPeriod
       && 0 <= tSettle' --p -- tSettle
       && tSettle p < tPeriod p -- tSettle p
       && 0 < rScanMin p -- rScanMin
       && rScanMin p <= rScanMax p -- rScanMax
       && rScanMax p < tPeriod p - tSettle' -- p -- rScanMax
       &&   tPeriod p + tSettle'  -- p 
          < rSampMin p -- rSampMin
       && rSampMin p <= rSampMax p -- rSampMax
       && rSampMax p <  -- p -- rSampMax
              2 * tPeriod p - rScanMax p - tSettle' 

