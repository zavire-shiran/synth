module Main where
import Data.Int
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.List (transpose)
import Data.Maybe
import qualified Data.Text as T
import System.IO (readFile)
import System.Random

type Signal = [Float]
type Instrument = SignalAttributes -> [[String]] -> Signal

nullSignal :: Signal
nullSignal = repeat 0

defaultSigAttr :: SignalAttributes
defaultSigAttr = SignalAttributes 44100

main :: IO ()
--main = let s1 = approxTriangleWave defaultSigAttr 1.0 (pitchToFrequency 60)
--           s2 = approxTriangleWave defaultSigAttr 1.0 (pitchToFrequency 64)
--           s3 = approxTriangleWave defaultSigAttr 1.0 (pitchToFrequency 67) in
--         BS.putStr . serializeSignal $ gain 4 $ sumSignals [s1, s2, s3]

main = BS.putStr . serializeSignal $ applyEnvelope defaultSigAttr 1.0 0.01 0.01 $ gain 3 $ genSquaretoothWave defaultSigAttr (addSignals (repeat $ pitchToFrequency 48) (gain 50 $ genSineWave defaultSigAttr (repeat $ pitchToFrequency 90)))
--main = BS.putStr . serializeSignal $ applyEnvelope defaultSigAttr 1.0 0.01 0.01 $ gain 3 $ lowPassFilter defaultSigAttr (pitchToFrequency 60) $ addSignals (genSquareWave defaultSigAttr (repeat $ pitchToFrequency 48)) (genSawtoothWave defaultSigAttr (repeat $ pitchToFrequency 48))
--main = BS.putStr . serializeSignal $ applyEnvelope defaultSigAttr 1.0 0.01 0.01 $ genSquaretoothWave defaultSigAttr (repeat $ pitchToFrequency 48)

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

genCompositeSineWave :: SignalAttributes -> [(Float, Float)] -> Signal
genCompositeSineWave sa wavedef = foldl addSignals nullSignal $ map makeSine_ wavedef
  where makeSine_ (f, g) = gain g $ genSineWave sa (repeat f)

wrap :: Float -> Float -> Float -> Float
wrap min max val =
  if val < min then
    val + (max - min)
  else if val > max then
    val - (max - min)
  else
    val

-- need convolve
-- with that, echo is simple

genRandomSignal :: Int -> Signal
genRandomSignal seed = randoms $ mkStdGen seed

-- There is an obvious refactor for these next functions

genSineWave :: SignalAttributes -> Signal -> Signal
genSineWave sa@(SignalAttributes sr) freq = inner 0 freq
            where inner phase (f:rest) = sin (phase * 2 * pi) : inner (wrap 0 1 (phase + f / sr)) rest

genTriangleWave :: SignalAttributes -> Signal -> Signal
genTriangleWave sa@(SignalAttributes sr) freq = inner 0 freq
            where inner phase (f:rest) = waveform phase : inner (wrap 0 1 (phase + f / sr)) rest
                  waveform p = case () of _
                                           | p < 0.25 -> p * 4
                                           | p < 0.75 -> 1 - ((p - 0.25) * 4)
                                           | otherwise -> (p-1) * 4

genSquareWave :: SignalAttributes -> Signal -> Signal
genSquareWave sa@(SignalAttributes sr) freq = inner 0 freq
  where inner phase (f:rest) = waveform phase : inner (wrap 0 1 (phase + f / sr)) rest
        waveform p = case () of _
                                 | p < 0.5 -> 1
                                 | otherwise -> -1

genSawtoothWave :: SignalAttributes -> Signal -> Signal
genSawtoothWave sa@(SignalAttributes sr) freq = inner 0 freq
  where inner phase (f:rest) = waveform phase : inner (wrap 0 1 (phase + f / sr)) rest
        waveform p = ((1 - p) - 0.5) * 2

genSquaretoothWave :: SignalAttributes -> Signal -> Signal
genSquaretoothWave sa@(SignalAttributes sr) freq = inner 0 freq
  where inner phase (f:rest) = waveform phase : inner (wrap 0 1 (phase + f / sr)) rest
        waveform p = case () of _
                                 | p < 0.5 -> 1 - p
                                 | otherwise -> p - 1.5

genEnvelope :: SignalAttributes -> Float -> Float -> Float -> Signal
genEnvelope sa@(SignalAttributes sr) len indur outdur = 0 : takeWhile (>0) (map (generateEnvelope sr len indur outdur) [1..])
  where generateEnvelope sr len indur outdur i = let curTime = i / sr in case () of _ | curTime < indur -> curTime / indur
                                                                                      | curTime < len - outdur -> 1.0
                                                                                      | otherwise  -> (len - curTime) / outdur

applyEnvelope :: SignalAttributes -> Float -> Float -> Float -> Signal -> Signal
applyEnvelope sa len indur outdur signal = multiplySignals signal (genEnvelope sa len indur outdur)

gain :: Float -> Signal -> Signal
gain g s = map (*g) s

addSignals :: Signal -> Signal -> Signal
addSignals one two = zipWith (+) one two

sumSignals :: [Signal] -> Signal
sumSignals = foldl addSignals nullSignal

multiplySignals :: Signal -> Signal -> Signal
multiplySignals one two = zipWith (*) one two

appendSignals :: Signal -> Signal -> Signal
appendSignals = (++)

lowPassFilter :: SignalAttributes -> Float -> Signal -> Signal
lowPassFilter sa@(SignalAttributes sr) cutoff input = 
  let rc = 1 / (2 * pi * cutoff)
      dt = 1 / sr
      alpha = dt / (rc + dt)
      f prev (sample:rest) = let s = (alpha * sample) + ((1-alpha) * prev) in s : f s rest in
    f 0 input

serializeSignal :: Signal -> BS.ByteString
serializeSignal signal = BS.concat $ map (Binary.encode . (truncate :: Float -> Int32) . (*1.5e8)) signal

makeTuples :: [a] -> [(a,a)]
makeTuples (x:y:xs) = (x,y) : makeTuples xs
makeTuples (x:[]) = []
makeTuples [] = []

sineInstr :: SignalAttributes -> Float -> [(Float, Float)] -> Signal
sineInstr sa length attrs = applyEnvelope sa length 0.01 0.01 $ genCompositeSineWave sa attrs

pitchToFrequency :: Float -> Float
pitchToFrequency pitch = 440 * (2 ** ((pitch - 69) / 12))

approxSawtoothWave :: SignalAttributes -> Float -> Float -> Signal
approxSawtoothWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1..20]

approxSquareWave :: SignalAttributes -> Float -> Float -> Signal
approxSquareWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1, 3..19]

approxTriangleWave :: SignalAttributes -> Float -> Float -> Signal
approxTriangleWave sa length freq = sineInstr sa length $ map (\(f, g) -> (f * freq, g)) harmonics
    where harmonics = [(1, 1), (3, -1/9), (5, 1/25), (7, -1/49), (9, 1/81), (11, -1/121)]

sineWave :: SignalAttributes -> Float -> Float -> Signal
sineWave sa length freq = sineInstr sa length [(freq, 1)]
