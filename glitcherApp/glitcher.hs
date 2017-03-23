import System.Environment 
import System.Random
import Control.Monad
import qualified Data.ByteString.Char8 as C

intToChar :: Int -> Char 
intToChar int = toEnum $ int `mod` 255

intToC :: Int -> C.ByteString 
intToC int = C.pack [intToChar int]

--split given bytestring, replace one of them, concat
replace :: Int -> Int -> C.ByteString -> C.ByteString 
replace location charVal bytes = mconcat [before,newChar,after]
    where (before,rest) = C.splitAt location bytes
          after = C.drop 1 rest
          newChar = intToC charVal

--pick a byte between 1..byteLength and replace it with 0..255
randomReplace :: C.ByteString -> IO C.ByteString 
randomReplace bytes = do
    let bytesLength = C.length bytes
    location <- randomRIO (1,bytesLength) 
    charVal <- randomRIO (0,255)
    return (replace location charVal bytes)

--similar to replaceByte
transpose :: Int -> Int -> C.ByteString -> C.ByteString 
transpose start size bytes = mconcat [before,changed,after]
    where (before,rest) = C.splitAt start bytes 
          (target,after) = C.splitAt size rest 
          changed = C.reverse $ C.sort target

randomTranspose :: C.ByteString -> IO C.ByteString 
randomTranspose bytes = do
    let sectionSize = 25
    let bytesLength = C.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (transpose start sectionSize bytes)

--delete bytestring
defect :: Int -> Int -> C.ByteString -> C.ByteString 
defect start size bytes = mconcat [before,emptyByte,after]
    where (before,rest) = C.splitAt start bytes 
          (target,after) = C.splitAt size rest
          emptyByte = C.drop (C.length target + 1) target

--randomly pick location to delete
randomDefect :: C.ByteString -> IO C.ByteString 
randomDefect bytes = do
    let sectionSize = 25
    let bytesLength = C.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (defect start sectionSize bytes)

--specify which and number of glitches 
imageGlitch :: [C.ByteString -> IO C.ByteString]
imageGlitch = [randomReplace
              ,randomTranspose
              ,randomDefect
              ]

--specify which and number of glitches 
imageGlitch2 :: [C.ByteString -> IO C.ByteString]
imageGlitch2 = [randomReplace
                ,randomReplace
                ,randomReplace
                ,randomReplace
                ,randomReplace
                ,randomReplace
                ]

glitcher :: FilePath -> IO ()
glitcher imageFileName = do
    imageFile <- C.readFile imageFileName
    glitched <- foldM (\bytes func -> func bytes) imageFile imageGlitch
    let glitchedFileName = mconcat ["glitched_",imageFileName]
    C.writeFile glitchedFileName glitched

glitcher2 :: FilePath -> IO ()
glitcher2 imageFileName = do
    imageFile <- C.readFile imageFileName
    glitched <- foldM (\bytes func -> func bytes) imageFile imageGlitch2
    let glitchedFileName = mconcat ["glitched_",imageFileName]
    C.writeFile glitchedFileName glitched

main :: IO ()
main = do
    print "pick a glitcher type. 1 is simple, anything else is replace*6"
    imageFiles <- getArgs
    glitcherType <- getLine
    if glitcherType == "1" 
    then mapM_ glitcher imageFiles
    else mapM_ glitcher2 imageFiles
    print "completed image glitching"