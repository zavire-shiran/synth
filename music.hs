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
--main = let s1 = allHarmonicsWave defaultSigAttr 1.0 (pitchToFrequency 60)
--           s2 = allHarmonicsWave defaultSigAttr 1.0 (pitchToFrequency 64)
--           s3 = allHarmonicsWave defaultSigAttr 1.0 (pitchToFrequency 67) in
--          BS.putStr . serializeSignal $ sumSignals [s1, s2, s3]

--main = BS.putStr . serializeSignal $ sineWave defaultSigAttr 1.0 (pitchToFrequency 80)

main = readComposition "composition" >>= print 

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

type Composition = [Sequence]

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

allHarmonicsWave :: SignalAttributes -> Float -> Float -> Signal
allHarmonicsWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1..20]

approxSquareWave :: SignalAttributes -> Float -> Float -> Signal
approxSquareWave sa length freq = sineInstr sa length $ map (\h -> (h * freq, 1/h)) [1, 3..19]

sineWave :: SignalAttributes -> Float -> Float -> Signal
sineWave sa length freq = sineInstr sa length [(freq, 10)]

--instrumentTable :: [(String, Instrument)]
--instrumentTable = [("approxSquare", approxSquareInstr)]

-- Sequences are series of events that are passed to a specific instrument
-- Events are one per line, no multiline events or multiples on one line
-- Compositions are a bunch of sequences with time offsets of each other
-- Each sequence of a composition is generated independently so that randomness gets redone

renderComposition :: Composition -> Signal
renderComposition composition = []

data Sequence = Sequence {
  instrument :: String,
  events :: [String],
  start :: Float
} deriving (Show)

readComposition :: String -> IO [Sequence]
readComposition filename = readFile filename >>= return . lines >>= readSequences

readSequences :: [String] -> IO [Sequence]
readSequences sequences = sequence $ map readSequence sequences

readSequence :: String -> IO Sequence
readSequence sequenceSpec = let specelems = words sequenceSpec
                                sequenceFile = specelems !! 0
                                start = specelems !! 1 in
                              readFile sequenceFile >>= return . lines >>= \ l -> return (Sequence (head l) (tail l) (read start))
