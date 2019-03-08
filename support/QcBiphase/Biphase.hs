{-
Lee Pike <leepike @ galois . com> (remove spaces)
Discrete-event type simulator for the Biphase Mark Protocol.
See https://leepike.github.io/pub_pages/qc-biphase.html and the associated paper.
BSD3 License.
-}

module Biphase where

-- A faster random-number generator
import System.Random.Mersenne

---------- DATATYPES ---------------------------------------
type Time = Double

-- | Realtime input parameters.
data Params = Params 
    { tPeriod  :: Time -- ^ Tx's clock period.
    , tSettle  :: Time -- ^ Nominal signal settling time.
    , rScanMin :: Time -- ^ Rx's min scan duration.
    , rScanMax :: Time -- ^ Rx's max scan duration.
    , rSampMin :: Time -- ^ Rx's min sampling duration.
    , rSampMax :: Time -- ^ Rx's max sampling duration.
    } deriving (Show, Eq)


data TState = SendFirst -- ^ Sending the 1st datum; 
            | SendSecond -- ^ Sending the 2nd.
              deriving (Show, Eq)

data Tx = Tx 
    { tstate   :: TState -- ^ Tx's state.
    , tsignal  :: Bool -- ^ Signal being sent.
    , tbit     :: Bool -- ^ Encoded bit to be sent.
    , changing :: Bool -- ^ T: modulating the signal; F o/w.
    , tclk     :: Time -- ^ Tx's timeout.
    } deriving (Show, Eq)

data RState = RcvFirst -- ^ Expecting the 1st datum; 
            | RcvSecond -- ^ Expecting the 2nd.
              deriving (Show, Eq)

data Rx = Rx 
    { rstate  :: RState -- ^ Rx's state.
    , rsignal :: Bool -- ^ Current datum being received.
    , rbit    :: Bool -- ^ Decoded bit.
    , rclk    :: Time -- ^ Rx's timeout.
    , synch   :: Bool -- ^ Rx just transitioned from 
                      -- RcvSecond to RcvFirst 
                      -- (capturing a bit).
    } deriving (Show, Eq)

------------------------------------------------------------

-- Helper for Mersenne randoms
randomRng :: (Time, Time) -> IO Time
randomRng (low, high) = do r <- randomIO
                           return $ low + (r * (high - low))

---------- INITIAL STATE/CLOCKS ----------------------------
initTx :: Params -> IO Tx
initTx p = do t <- randomRng (0, tPeriod p - tSettle p) 
              bit <- randomIO
              return Tx { tstate = SendFirst
                        , tsignal = True
                        , tbit = bit
                        , changing = False
                        , tclk = t}

initRclock :: Params -> IO Time
initRclock p = do r <- randomRng (0, rScanMax p)
                  -- we want a random in [a, a)
                  if r == rScanMax p 
                    then initRclock p
                    else return r

initRx :: Params -> IO Rx
initRx p = do r <- initRclock p
              bit <- randomIO
              return Rx { rstate = RcvFirst
                        , rsignal = True
                        , rbit = bit
                        , rclk = r
                        , synch = False}
------------------------------------------------------------

---------- Tx UPDATE ---------------------------------------
-- | 
tenv :: Tx -> IO Tx
tenv tx = case tstate tx of 
            SendFirst -> do ran <- randomIO 
                            return tx {tbit = ran} 
            SendSecond -> return tx

-- | The transmitter's encoder.  Protocol-specific.
tenc :: Tx -> IO Tx
tenc tx = 
    case tstate tx of
      SendFirst -> 
          do idle <- randomIO 
             if idle -- Idling
                then return tx {changing = False} 
                -- 1st half of a new bit.
                else return 
                          tx { tsignal = ttoggle
                              , tstate = SendSecond
                              , changing = True}

      SendSecond -> return tx { tsignal = toggle  
                              , tstate = SendFirst
                              , changing = changed toggle}
    where toggle = if tbit tx 
                   then ttoggle else tsignal tx
          ttoggle = not $ tsignal tx
          changed cur = cur /= tsignal tx
  
tclock :: Params -> Tx -> Tx
tclock p tx = tx {tclk = tPeriod p + tclk tx}

txUpdate :: Params -> Tx -> IO Tx
txUpdate p tx = do
  tx' <- tenv tx
  tx'' <- tenc tx'
  return $ tclock p tx''
------------------------------------------------------------

---------- Rx UPDATE ---------------------------------------
-- | Correct update of rclk---helper
rclock :: Params -> Rx -> IO Time
rclock p rx = 
    let r = rclk rx
    in case rstate rx of
         RcvFirst ->  
             randomRng (r + rScanMin p, r + rScanMax p)
         RcvSecond -> 
             randomRng (r + rSampMin p, r + rSampMax p) 

stable :: Params -> Rx -> Tx -> Bool
stable p rx tx =    
     not (changing tx) 
  || tclk tx - rclk rx < tPeriod p - tSettle p

-- | The receiver's decoder.  Protocol-specific.
rdec :: Params -> Rx -> Tx -> Rx
rdec p rx tx = 
      -- Are we in a "stable" part of the signal?
  let badSignal = not $ tsignal tx 
      v         = if stable p rx tx
                  then tsignal tx else badSignal 
  in case rstate rx of 
       RcvSecond ->  rx { rsignal = v
                        , rbit = rsignal rx /= v
                        , rstate = RcvFirst}
       RcvFirst  -> rx { rsignal = v
                       , rstate = signal}
           where signal = if v == rsignal rx 
                          then RcvFirst
                          else RcvSecond
                               
rxUpdate :: Params -> Rx -> Tx -> IO Rx
rxUpdate p rx tx = do 
  let rx'     = rdec p rx tx 
      rchange = case (rstate rx, rstate rx') of
                  (RcvSecond, RcvFirst) -> True
                  _                     -> False
  r <- rclock p rx'
  return rx' { rclk = r
             , synch = rchange} 
------------------------------------------------------------

-- | Full state transition.
transition :: Params -> (Rx, Tx) -> IO (Rx, Tx)
transition p (rx, tx)
  | tclk tx <= rclk rx = do 
      tx' <- txUpdate p tx
      return (rx {synch = False}, tx')
  | otherwise = do 
      rx' <- rxUpdate p rx tx
      return (rx', tx)

putLnState :: Integer -> (Rx, Tx) -> IO ()
putLnState i (rx, tx) = do 
  putStrLn $ "States: " ++ (show $ tstate tx) ++ " " 
               ++ (show $ rstate rx)
  putStrLn $ "Clocks: " 
               ++ (show $ tclk tx) ++ " " 
               ++ (show $ rclk rx)
  putStrLn $ "Bits: " 
               ++ (show $ tbit tx) ++ " " 
               ++ (show $ rbit rx) 
               ++ " Signal: " ++ (show $ tsignal tx) 
               ++ " " ++ (show $ rsignal rx)
  putStrLn $ "i: " ++ (show i) ++ " Synch: " 
               ++ (show $ synch rx) ++ "\n"

-- | Defines a "good" stop state: tx has sent the 2nd 
-- signal bit and rx has sampled it.
stopState :: Rx -> Bool
stopState rx = synch rx

execToStopState :: Bool -> Params -> Integer -> (Rx, Tx) -> IO (Rx, Tx)
execToStopState output p i s = do
    if output then putLnState i s else return ()
    if stopState (fst s) 
      then return s
      else execToStopState output p i =<< transition p s

-- | Exectuion of the protocol.
exec :: Bool -> Params -> Integer -> (Rx, Tx) -> IO (Rx, Tx)
exec output p i s = do
  s' <- execToStopState output p i s
  if i < 1 then return s'
           else exec output p (i-1) s' 

-- | Begin a finite trace of length i from the initial 
-- state. Either send one determined signal bit or a 
-- series of nondeterministic signals.
startExec :: Bool -> Params -> Integer -> IO (Rx, Tx)
startExec output p i = exec output p i =<< initState p

-- | The initial state.
initState :: Params -> IO (Rx, Tx)
initState p = do 
  rx <- initRx p 
  tx <- initTx p
  return (rx, tx)
