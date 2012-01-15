module Main where
import Data.Int
import Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS

type Signal = [Float]

nullSignal :: Signal
nullSignal = repeat 0

defaultSigAttr :: SignalAttributes
defaultSigAttr = SignalAttributes 44100

main :: IO ()
main = let signal1 = applyEnvelope defaultSigAttr 1 0.01 0.01 (genCompositeSineWave defaultSigAttr [(220, 1.0), (440, 0.5), (880,  0.25), (1320, 0.125)])
           signal2 = applyEnvelope defaultSigAttr 1 0.01 0.01 (genCompositeSineWave defaultSigAttr [(440, 1.0), (880, 0.5), (1320, 0.25), (1760, 0.125)]) in
  BS.putStr $ serializeSignal $ appendSignals signal1 signal2

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

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
