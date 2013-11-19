module Main where
import Data.Int
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.List (transpose)
import Data.Maybe
import qualified Data.Text as T
import System.IO (readFile)

-- Stolen shamelessly from some hackage library
join :: String -> [String] -> String
join _ [] = ""
join _ [a] = a
join sep (x:xs) = x ++ sep ++ join sep xs

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

main = BS.putStr . serializeSignal $ applyEnvelope defaultSigAttr 1.0 0.01 0.01 $ gain 3 $ genTriangleWave defaultSigAttr (addSignals (repeat $ pitchToFrequency 48) (gain 100 $ genSineWave defaultSigAttr (repeat $ pitchToFrequency 90)))

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
