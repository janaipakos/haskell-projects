import System.Environment 
import System.Random
import Control.Monad
import qualified Data.ByteString.Char8 as C

intToChar :: Int -> Char 
intToChar int = toEnum $ int `mod` 255

intToC :: Int -> C.ByteString 
intToC int = C.pack [intToChar int]

--split given bytestring, replace one of them, concat
byteReplace :: Int -> Int -> C.ByteString -> C.ByteString 
byteReplace location charVal bytes = mconcat [before,newChar,after]
    where (before,rest) = C.splitAt location bytes
          after = C.drop 1 rest
          newChar = intToC charVal

--pick a byte between 1..byteLength and replace it with 0..255
byteReplaceRandom :: C.ByteString -> IO C.ByteString 
byteReplaceRandom bytes = do
    let bytesLength = C.length bytes
    location <- randomRIO (1,bytesLength) 
    charVal <- randomRIO (0,255)
    return (byteReplace location charVal bytes)

--similar to replaceByte
sectionSort :: Int -> Int -> C.ByteString -> C.ByteString 
sectionSort start size bytes = mconcat [before,changed,after]
    where (before,rest) = C.splitAt start bytes 
          (target,after) = C.splitAt size rest 
          changed = C.reverse $ C.sort target

sectionSortRandom :: C.ByteString -> IO C.ByteString 
sectionSortRandom bytes = do
    let sectionSize = 25
    let bytesLength = C.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sectionSort start sectionSize bytes)

imageGlitch :: [C.ByteString -> IO C.ByteString]
imageGlitch = [byteReplaceRandom
              ,sectionSortRandom
              ]

glitcher :: FilePath -> IO ()
glitcher imageFileName = do
    imageFile <- C.readFile imageFileName
    glitched <- foldM (\bytes func -> func bytes) imageFile imageGlitch
    let glitchedFileName = mconcat ["glitched_",imageFileName]
    C.writeFile glitchedFileName glitched

main :: IO ()
main = do
    imageFiles <- getArgs
    mapM_ glitcher imageFiles
    print "completed image glitching"