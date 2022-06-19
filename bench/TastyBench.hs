{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TastyBench where

import Prelude hiding (Int, Integer)
import qualified Prelude
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Data (Typeable)
import Data.Word (Word64)
#if !MIN_VERSION_base(4,10,0)
import Data.Int ( Int64 )
#endif
#if MIN_VERSION_base(4,6,0)
import GHC.Stats
#endif
import System.CPUTime
import System.IO
import System.IO.Unsafe
import System.Mem
import Text.Printf (printf)


data Timeout
  = Timeout
    Prelude.Integer -- ^ number of microseconds (e. g., 200000)
    String          -- ^ textual representation (e. g., @"0.2s"@)
  | NoTimeout
  deriving (Show)


-- | In addition to @--stdev@ command-line option,
-- one can adjust target relative standard deviation
-- for individual benchmarks and groups of benchmarks
-- using 'adjustOption' and 'localOption'.
--
-- E. g., set target relative standard deviation to 2% as follows:
--
-- > import Test.Tasty (localOption)
-- > localOption (RelStDev 0.02) (bgroup [...])
--
-- If you set 'RelStDev' to infinity,
-- a benchmark will be executed
-- only once and its standard deviation will be recorded as zero.
-- This is rather a blunt approach, but it might be a necessary evil
-- for extremely long benchmarks. If you wish to run all benchmarks
-- only once, use command-line option @--stdev@ @Infinity@.
--
-- @since 0.2
newtype RelStDev = RelStDev Double
  deriving (Show, Read, Typeable)

-- | Whether to measure CPU time or wall-clock time.
-- Normally 'CpuTime' is a better option (and default),
-- but consider switching to 'WallTime'
-- to measure multithreaded algorithms or time spent in external processes.
--
-- One can switch the default measurement mode globally
-- using @--time-mode@ command-line option,
-- but it is usually better to adjust the mode locally:
--
-- > import Test.Tasty (localOption)
-- > localOption WallTime (bgroup [...])
--
-- @since 0.3.2
data TimeMode = CpuTime
  -- ^ Measure CPU time.
  deriving (Typeable)

-- | Something that can be benchmarked, produced by 'nf', 'whnf', 'nfIO', 'whnfIO',
-- 'nfAppIO', 'whnfAppIO' below.
--
-- Drop-in replacement for @Criterion.@'Criterion.Benchmarkable' and
-- @Gauge.@'Gauge.Benchmarkable'.
--
-- @since 0.1
newtype Benchmarkable =
    -- | @since 0.3
    Benchmarkable
  { unBenchmarkable :: Word64 -> IO () -- ^ Run benchmark given number of times.
  } deriving (Typeable)


data Measurement = Measurement
  { measTime   :: !Word64 -- ^ time in picoseconds
  , measAllocs :: !Word64 -- ^ allocations in bytes
  , measCopied :: !Word64 -- ^ copied bytes
  , measMaxMem :: !Word64 -- ^ max memory in use
  } deriving (Show, Read)

data Estimate = Estimate
  { estMean  :: !Measurement
  , estStdev :: !Word64  -- ^ stdev in picoseconds
  } deriving (Show, Read)


predict
  :: Measurement -- ^ time for one run
  -> Measurement -- ^ time for two runs
  -> Estimate
predict (Measurement t1 a1 c1 m1) (Measurement t2 a2 c2 m2) = Estimate
  { estMean  = Measurement t (fit a1 a2) (fit c1 c2) (max m1 m2)
  , estStdev = truncate (sqrt d :: Double)
  }
  where
    fit x1 x2 = x1 `quot` 5 + 2 * (x2 `quot` 5)
    t = fit t1 t2
    sqr x = x * x
    d = sqr (word64ToDouble t1 -     word64ToDouble t)
      + sqr (word64ToDouble t2 - 2 * word64ToDouble t)

predictPerturbed :: Measurement -> Measurement -> Estimate
predictPerturbed t1 t2 = Estimate
  { estMean = estMean (predict t1 t2)
  , estStdev = max
    (estStdev (predict (lo t1) (hi t2)))
    (estStdev (predict (hi t1) (lo t2)))
  }
  where
    prec = max (fromInteger cpuTimePrecision) 1000000000 -- 1 ms
    hi meas = meas { measTime = measTime meas + prec }
    lo meas = meas { measTime = measTime meas - prec }

hasGCStats :: Bool
#if MIN_VERSION_base(4,10,0)
hasGCStats = unsafePerformIO getRTSStatsEnabled
#elif MIN_VERSION_base(4,6,0)
hasGCStats = unsafePerformIO getGCStatsEnabled
#else
hasGCStats = False
#endif

getAllocsAndCopied :: IO (Word64, Word64, Word64)
getAllocsAndCopied = do
  if not hasGCStats then pure (0, 0, 0) else
#if MIN_VERSION_base(4,10,0)
    (\s -> (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s)) <$> getRTSStats
#elif MIN_VERSION_base(4,6,0)
    (\s -> (int64ToWord64 $ bytesAllocated s, int64ToWord64 $ bytesCopied s, int64ToWord64 $ peakMegabytesAllocated s * 1024 * 1024)) <$> getGCStats
#else
    pure (0, 0, 0)
#endif

getTimePicoSecs :: TimeMode -> IO Word64
getTimePicoSecs timeMode = case timeMode of
  CpuTime -> fromInteger <$> getCPUTime

measure :: TimeMode -> Word64 -> Benchmarkable -> IO Measurement
measure timeMode n (Benchmarkable act) = do
  let getTimePicoSecs' = getTimePicoSecs timeMode
  performGC
  startTime <- getTimePicoSecs'
  (startAllocs, startCopied, startMaxMemInUse) <- getAllocsAndCopied
  act n
  endTime <- getTimePicoSecs'
  (endAllocs, endCopied, endMaxMemInUse) <- getAllocsAndCopied
  let meas = Measurement
        { measTime   = endTime - startTime
        , measAllocs = endAllocs - startAllocs
        , measCopied = endCopied - startCopied
        , measMaxMem = max endMaxMemInUse startMaxMemInUse
        }
  pure meas

measureUntil :: TimeMode -> Bool -> Timeout -> RelStDev -> Benchmarkable -> IO Estimate
measureUntil timeMode _ _ (RelStDev targetRelStDev) b
  | isInfinite targetRelStDev, targetRelStDev > 0 = do
  t1 <- measure timeMode 1 b
  pure $ Estimate { estMean = t1, estStdev = 0 }
measureUntil timeMode warnIfNoTimeout timeout (RelStDev targetRelStDev) b = do
  t1 <- measure' 1 b
  go 1 t1 0
  where
    measure' = measure timeMode

    go :: Word64 -> Measurement -> Word64 -> IO Estimate
    go n t1 sumOfTs = do
      t2 <- measure' (2 * n) b

      let Estimate (Measurement meanN allocN copiedN maxMemN) stdevN = predictPerturbed t1 t2
          isTimeoutSoon = case timeout of
            NoTimeout -> False
            -- multiplying by 12/10 helps to avoid accidental timeouts
            Timeout micros _ -> (sumOfTs' + 3 * measTime t2) `quot` (1000000 * 10 `quot` 12) >= fromInteger micros
          isStDevInTargetRange = stdevN < truncate (max 0 targetRelStDev * word64ToDouble meanN)
          scale = (`quot` n)
          sumOfTs' = sumOfTs + measTime t1

      case timeout of
        NoTimeout | warnIfNoTimeout, sumOfTs' + measTime t2 > 100 * 1000000000000
          -> hPutStrLn stderr "This benchmark takes more than 100 seconds. Consider setting --timeout, if this is unexpected (or to silence this warning)."
        _ -> pure ()

      if isStDevInTargetRange || isTimeoutSoon
        then pure $ Estimate
          { estMean  = Measurement (scale meanN) (scale allocN) (scale copiedN) maxMemN
          , estStdev = scale stdevN }
        else go (2 * n) t2 sumOfTs'

-- | An internal routine to measure CPU execution time in seconds
-- for a given timeout (put 'NoTimeout', or 'mkTimeout' 100000000 for 100 seconds)
-- and a target relative standard deviation
-- (put 'RelStDev' 0.05 for 5% or 'RelStDev' (1/0) to run only one iteration).
--
-- 'Timeout' takes soft priority over 'RelStDev': this function prefers
-- to finish in time even if at cost of precision. However, timeout is guidance
-- not guarantee: 'measureCpuTime' can take longer, if there is not enough time
-- to run at least thrice or an iteration takes unusually long.
--
-- @since 0.3
measureCpuTime :: Timeout -> RelStDev -> Benchmarkable -> IO Double
measureCpuTime
    = ((fmap ((/ 1e12) . word64ToDouble . measTime . estMean) .) .)
    . measureUntil CpuTime False



funcToBench :: (b -> c) -> (a -> b) -> a -> Benchmarkable
funcToBench frc = (Benchmarkable .) . benchLoop
  where
    -- Here we rely on the fact that GHC (unless spurred by
    -- -fstatic-argument-transformation) is not smart enough:
    -- it does not notice that `f` and `x` arguments are loop invariant
    -- and could be floated, and the whole `f x` expression shared.
    -- If we create a closure with `f` and `x` bound in the environment,
    -- then GHC is smart enough to share computation of `f x`.
    --
    -- For perspective, gauge and criterion < 1.4 mark similar functions as INLINE,
    -- while criterion >= 1.4 switches to NOINLINE.
    -- If we mark `benchLoop` NOINLINE then benchmark results are slightly larger
    -- (noticeable in bench-fibo), because the loop body is slightly bigger,
    -- since GHC does not unbox numbers or inline `Eq @Word64` dictionary.
    --
    -- This function is called `benchLoop` instead of, say, `go`,
    -- so it is easier to spot in Core dumps.
    benchLoop f x n
      | n == 0    = pure ()
      | otherwise = do
        _ <- evaluate (frc (f x))
        benchLoop f x (n - 1)
{-# INLINE funcToBench #-}

-- | 'nf' @f@ @x@ measures time to compute
-- a normal form (by means of 'force') of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Here is a textbook anti-pattern: 'nf' 'sum' @[1..1000000]@.
-- Since an input list is shared by multiple invocations of 'sum',
-- it will be allocated in memory in full, putting immense pressure
-- on garbage collector. Also no list fusion will happen.
-- A better approach is 'nf' (@\\n@ @->@ 'sum' @[1..n]@) @1000000@.
--
-- If you are measuring an inlinable function,
-- it is prudent to ensure that its invocation is fully saturated,
-- otherwise inlining will not happen. That's why one can often
-- see 'nf' (@\\n@ @->@ @f@ @n@) @x@ instead of 'nf' @f@ @x@.
-- Same applies to rewrite rules.
--
-- While @tasty-bench@ is capable to perform micro- and even nanobenchmarks,
-- such measurements are noisy and involve an overhead. Results are more reliable
-- when @f@ @x@ takes at least several milliseconds.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios (imagine benchmarking 'tail'),
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- Drop-in replacement for @Criterion.@'Criterion.nf' and
-- @Gauge.@'Gauge.nf'.
--
-- @since 0.1
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = funcToBench force
{-# INLINE nf #-}

-- | 'whnf' @f@ @x@ measures time to compute
-- a weak head normal form of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Beware that many educational materials contain examples with 'whnf':
-- this is a wrong default.
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nf' instead.
--
-- Here is a textbook anti-pattern: 'whnf' ('Data.List.replicate' @1000000@) @1@.
-- This will succeed in a matter of nanoseconds, because weak head
-- normal form forces only the first element of the list.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnf' and @Gauge.@'Gauge.whnf'.
--
-- @since 0.1
whnf :: (a -> b) -> a -> Benchmarkable
whnf = funcToBench id
{-# INLINE whnf #-}

ioToBench :: (b -> c) -> IO b -> Benchmarkable
ioToBench frc act = Benchmarkable go
  where
    go n
      | n == 0    = pure ()
      | otherwise = do
        val <- act
        _ <- evaluate (frc val)
        go (n - 1)
{-# INLINE ioToBench #-}

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its normal form (by means of 'force').
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'nfAppIO' instead.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfIO' ('readFile' @"foo.txt"@).
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.nfIO' and @Gauge.@'Gauge.nfIO'.
--
-- @since 0.1
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = ioToBench force
{-# INLINE nfIO #-}

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its weak head normal form.
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'whnfAppIO' instead.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnfIO' and @Gauge.@'Gauge.whnfIO'.
--
-- @since 0.1
whnfIO :: IO a -> Benchmarkable
whnfIO = ioToBench id
{-# INLINE whnfIO #-}

ioFuncToBench :: (b -> c) -> (a -> IO b) -> a -> Benchmarkable
ioFuncToBench frc = (Benchmarkable .) . go
  where
    go f x n
      | n == 0    = pure ()
      | otherwise = do
        val <- f x
        _ <- evaluate (frc val)
        go f x (n - 1)
{-# INLINE ioFuncToBench #-}

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its normal form (by means of 'force').
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfAppIO' 'readFile' @"foo.txt"@.
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.nfAppIO' and @Gauge.@'Gauge.nfAppIO'.
--
-- @since 0.1
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = ioFuncToBench force
{-# INLINE nfAppIO #-}

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfAppIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnfAppIO' and @Gauge.@'Gauge.whnfAppIO'.
--
-- @since 0.1
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = ioFuncToBench id
{-# INLINE whnfAppIO #-}


word64ToDouble :: Word64 -> Double
word64ToDouble = fromIntegral

#if !MIN_VERSION_base(4,10,0) && MIN_VERSION_base(4,6,0)
int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral
#endif

prettyEstimate :: Estimate -> String
prettyEstimate (Estimate m stdev) =
  showPicos4 (measTime m)
  ++ (if stdev == 0 then "         " else " ± " ++ showPicos3 (2 * stdev))

-- | Show picoseconds, fitting number in 4 characters.
showPicos4 :: Word64 -> String
showPicos4 i
  | t < 995   = printf "%3.0f  ps"  t
  | t < 995e1 = printf "%4.2f ns"  (t / 1e3)
  | t < 995e2 = printf "%4.1f ns"  (t / 1e3)
  | t < 995e3 = printf "%3.0f  ns" (t / 1e3)
  | t < 995e4 = printf "%4.2f μs"  (t / 1e6)
  | t < 995e5 = printf "%4.1f μs"  (t / 1e6)
  | t < 995e6 = printf "%3.0f  μs" (t / 1e6)
  | t < 995e7 = printf "%4.2f ms"  (t / 1e9)
  | t < 995e8 = printf "%4.1f ms"  (t / 1e9)
  | t < 995e9 = printf "%3.0f  ms" (t / 1e9)
  | otherwise = printf "%4.3f s"   (t / 1e12)
  where
    t = word64ToDouble i

-- | Show picoseconds, fitting number in 3 characters.
showPicos3 :: Word64 -> String
showPicos3 i
  | t < 995   = printf "%3.0f ps" t
  | t < 995e1 = printf "%3.1f ns" (t / 1e3)
  | t < 995e3 = printf "%3.0f ns" (t / 1e3)
  | t < 995e4 = printf "%3.1f μs" (t / 1e6)
  | t < 995e6 = printf "%3.0f μs" (t / 1e6)
  | t < 995e7 = printf "%3.1f ms" (t / 1e9)
  | t < 995e9 = printf "%3.0f ms" (t / 1e9)
  | otherwise = printf "%4.2f s"  (t / 1e12)
  where
    t = word64ToDouble i

prettyEstimateWithGC :: Estimate -> String
prettyEstimateWithGC (Estimate m stdev) =
  showPicos4 (measTime m)
  ++ (if stdev == 0 then ",          " else " ± " ++ showPicos3 (2 * stdev) ++ ", ")
  ++ showBytes (measAllocs m) ++ " allocated, "
  ++ showBytes (measCopied m) ++ " copied, "
  ++ showBytes (measMaxMem m) ++ " peak memory"

csvEstimate :: Estimate -> String
csvEstimate (Estimate m stdev) = show (measTime m) ++ "," ++ show (2 * stdev)

csvEstimateWithGC :: Estimate -> String
csvEstimateWithGC (Estimate m stdev) = show (measTime m) ++ "," ++ show (2 * stdev)
  ++ "," ++ show (measAllocs m) ++ "," ++ show (measCopied m) ++ "," ++ show (measMaxMem m)

showBytes :: Word64 -> String
showBytes i
  | t < 1000                 = printf "%3.0f B " t
  | t < 10189                = printf "%3.1f KB" (t / 1024)
  | t < 1023488              = printf "%3.0f KB" (t / 1024)
  | t < 10433332             = printf "%3.1f MB" (t / 1048576)
  | t < 1048051712           = printf "%3.0f MB" (t / 1048576)
  | t < 10683731149          = printf "%3.1f GB" (t / 1073741824)
  | t < 1073204953088        = printf "%3.0f GB" (t / 1073741824)
  | t < 10940140696372       = printf "%3.1f TB" (t / 1099511627776)
  | t < 1098961871962112     = printf "%3.0f TB" (t / 1099511627776)
  | t < 11202704073084108    = printf "%3.1f PB" (t / 1125899906842624)
  | t < 1125336956889202624  = printf "%3.0f PB" (t / 1125899906842624)
  | t < 11471568970838126592 = printf "%3.1f EB" (t / 1152921504606846976)
  | otherwise                = printf "%3.0f EB" (t / 1152921504606846976)
  where
    t = word64ToDouble i
