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
main = let s1 = approxSquareWave defaultSigAttr 1.0 (pitchToFrequency 60)
           s2 = approxSquareWave defaultSigAttr 1.0 (pitchToFrequency 62) in
          BS.putStr . serializeSignal $ (appendSignals s1 s2)

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

type Composition = [Sequence]
data Sequence = Sequence Instrument [[String]] Float

genCompositeSineWave :: SignalAttributes -> [(Float, Float)] -> Signal
genCompositeSineWave sa wavedef = foldl addSignals nullSignal $ map makeSine_ wavedef
  where makeSine_ (f, g) = gain g $ genSineWave sa f

genSineWave :: SignalAttributes -> Float -> Signal
genSineWave sa@(SignalAttributes sr) freq = map (sin . (* (pi * freq / sr))) [0..]

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

allHarmonicsWave :: SignalAttributes -> Float -> Float -> Signal
allHarmonicsWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1..20]

approxSquareWave :: SignalAttributes -> Float -> Float -> Signal
approxSquareWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1, 3..19]

--instrumentTable :: [(String, Instrument)]
--instrumentTable = [("approxSquare", approxSquareInstr)]
