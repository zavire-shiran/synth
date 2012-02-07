module Main where
import Data.Int
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.List (transpose)
import Data.Maybe
import qualified Data.Text as T
import System.IO (readFile)

type Signal = [Float]
type Instrument = SignalAttributes -> [Float] -> Signal

nullSignal :: Signal
nullSignal = repeat 0

defaultSigAttr :: SignalAttributes
defaultSigAttr = SignalAttributes 44100

main :: IO ()
main =
 readComposition "composition" >>= BS.putStr . serializeSignal . renderComposition defaultSigAttr

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

type Composition = [Sequence]
data Sequence = Sequence Instrument [[Float]] Float

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

serializeSignal :: [Float] -> BS.ByteString
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

approxSquareInstr :: Instrument
approxSquareInstr sa [d, l, f] = delay sa d $ approxSquareWave sa l $ pitchToFrequency f

delay :: SignalAttributes -> Float -> Signal -> Signal
delay sa@(SignalAttributes sr) d = appendSignals (take (truncate (sr * d)) nullSignal)

renderInstrument :: SignalAttributes -> Instrument -> [[Float]] -> Signal
renderInstrument sa instr arguments =
  let notes = map (instr sa) arguments in
    map sum $ transpose notes

readSection :: String -> IO [[Float]]
readSection fileName = readFile fileName >>= return . filter (not . null) . map (map read) . map words . lines

instrumentTable :: [(String, Instrument)]
instrumentTable = [("approxSquare", approxSquareInstr)]

makeSequence :: [String] -> IO Sequence
makeSequence [name, instr, delay] = readSection name >>= \section -> return $ Sequence (fromJust $ lookup instr instrumentTable) section (read delay)

readComposition :: String -> IO Composition
readComposition filename = readFile filename >>= sequence . map makeSequence . map words . lines

renderSequence :: SignalAttributes -> Sequence -> Signal
renderSequence sa (Sequence instr arguments d) = delay sa d $ renderInstrument sa instr arguments

renderComposition :: SignalAttributes -> Composition -> Signal
renderComposition sa = map sum . transpose . map (renderSequence sa)
