module Main where
import Data.Int
import Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS

type Signal = [Float]

nullSignal :: Signal
nullSignal = repeat 0


main :: IO ()
main = let signal = genCompositeSineWave (SignalAttributes 44100) [(440, 1.0), (880, 0.5), (1320, 0.25), (1760, 0.125)] in
  BS.putStr $ serializeSignal $ multiplySignals signal (genEnvelope (SignalAttributes 44100) 2 0.01 0.01)

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

genCompositeSineWave :: SignalAttributes -> [(Float, Float)] -> Signal
genCompositeSineWave sa wavedef = foldl addSignals nullSignal $ map makeSine_ wavedef
  where makeSine_ (f, g) = gain g $ genSineWave sa f

genSineWave :: SignalAttributes -> Float -> Signal
genSineWave sa@(SignalAttributes sr) freq = map (sin . (* (pi * freq / sr))) [0..]

genEnvelope :: SignalAttributes -> Float -> Float -> Float -> [Float]
genEnvelope sa@(SignalAttributes sr) len indur outdur = 0 : takeWhile (>0) (map (generateEnvelope sr len indur outdur) [1..])
  where generateEnvelope sr len indur outdur i = let curTime = i / sr in case () of _ | curTime < indur -> curTime / indur
                                                                                      | curTime < len - outdur -> 1.0
                                                                                      | otherwise  -> (len - curTime) / outdur

gain :: Float -> Signal -> Signal
gain g s = map (*g) s

addSignals :: Signal -> Signal -> Signal
addSignals one two = zipWith (+) one two

multiplySignals :: Signal -> Signal -> Signal
multiplySignals one two = zipWith (*) one two

serializeSignal :: [Float] -> BS.ByteString
serializeSignal signal = BS.concat $ map (Binary.encode . (truncate :: Float -> Int32) . (*1.5e8)) signal
