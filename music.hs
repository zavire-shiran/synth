module Main where
import Data.Int
import Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = BS.putStr $ serializeSignal $ take 44100 $ genSineWave (SignalAttributes 44100) 440

data SignalFormat = UnsignedInteger | Float

data SignalAttributes = SignalAttributes {
  sampleRate :: Float
}

genSineWave :: SignalAttributes -> Float -> [Float]
genSineWave sa@(SignalAttributes sr) freq = generateSineWave sr freq 0
  where generateSineWave sr freq theta = sin theta : (generateSineWave sr freq $ theta + (pi * freq / sr))

serializeSignal :: [Float] -> BS.ByteString
serializeSignal signal = BS.concat $ map (Binary.encode . (truncate :: Float -> Int32) . (*1e9)) signal
