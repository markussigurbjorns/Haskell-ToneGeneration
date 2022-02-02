module Main where

import Data.Monoid ()
import Data.Foldable (fold, foldMap)
import System.IO ( stdout )
import System.Process
import Data.Int (Int16)
import Data.ByteString.Lazy.Builder
    ( int16LE, toLazyByteString, Builder )
import Data.ByteString.Lazy (ByteString, writeFile)
import Text.Printf (printf)

outputFile :: String
outputFile = "output.bin"

concertA :: Double
concertA = 440.0

wave :: [Double]
wave = map sin [0 .. 44100]

sineWave :: Double -> Double
sineWave t = sin (t * 2 * pi)

applyPitch :: Double -> (Double -> Double) -> (Double -> Double)
applyPitch pitch orig t = orig (t * pitch)

audioFn :: Double -> Double
audioFn = applyPitch concertA sineWave

sampleTimes :: Double -> Double -> Double -> [Double]
sampleTimes frequency start end
    | start > end = []
    | otherwise = start:sampleTimes frequency (start + (1.0 / frequency)) end

sample :: (Double -> Double) -> Double -> Double -> [Double]
sample audioFn frequency duration = [audioFn t | t <- sampleTimes frequency 0 duration]

sampleInt16 :: (Double -> Double) -> Double -> Double -> [Int16]
sampleInt16 audioFn frequency duration =
    [floor (v * 32767.5) | v <- samples]
    where samples = sample audioFn frequency duration

samples :: [Int16]
samples = sampleInt16 audioFn 44100.0 5

audioBuilder :: Builder
audioBuilder = mconcat sampleBuilders
    where sampleBuilders = map int16LE samples

saveFile :: IO ()
saveFile = Data.ByteString.Lazy.writeFile outputFile $ toLazyByteString audioBuilder

play :: IO ()
play = do
    saveFile
    _ <- runCommand $ printf "ffplay -f s16le -ar 44100 output.bin" 
    return ()

    
main :: IO ()
main = play


