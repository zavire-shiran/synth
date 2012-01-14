module Main where
import Data.Int
import Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS

type Signal = [Float]

main :: IO ()
main = BS.putStr $ serializeSignal $ multiplySignals (genSineWave (SignalAttributes 44100) 440) (genEnvelope (SignalAttributes 44100) 2 0.01 0.01)

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

genSineWave :: SignalAttributes -> Float -> [Float]
genSineWave sa@(SignalAttributes sr) freq = generateSineWave sr freq 0
  where generateSineWave sr freq theta = sin theta : (generateSineWave sr freq $ theta + (pi * freq / sr))

genEnvelope :: SignalAttributes -> Float -> Float -> Float -> [Float]
genEnvelope sa@(SignalAttributes sr) len indur outdur = 0 : takeWhile (>0) (map (generateEnvelope sr len indur outdur) [1..])
  where generateEnvelope sr len indur outdur i = let curTime = i / sr in case () of _ | curTime < indur -> curTime / indur
                                                                                      | curTime < len - outdur -> 1.0
                                                                                      | otherwise  -> (len - curTime) / outdur

multiplySignals :: Signal -> Signal -> Signal
multiplySignals one two = zipWith (*) one two

serializeSignal :: [Float] -> BS.ByteString
serializeSignal signal = BS.concat $ map (Binary.encode . (truncate :: Float -> Int32) . (*1e9)) signal
