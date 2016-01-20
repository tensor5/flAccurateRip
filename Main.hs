import           Control.DeepSeq
import           Control.Monad
import           Data.Binary
import           Data.Bits
import           Data.String
import           Data.Version
import           Network.HTTP
import           Options.Applicative
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

import           CD.AccurateRip
import           CD.CDDB

import           Paths_flaccuraterip

progName :: String
progName = "flAccurateRip"

intro :: String
intro = progName ++ " " ++ showVersion version ++
        "\n\
        \Copyright (C) 2012-2016 Nicola Squartini.\n\
        \License GPLv3+: GNU GPL version 3 or \
        \later <http://gnu.org/licenses/gpl.html>\n\
        \This is free software: you are free to change and redistribute it.\n\
        \There is NO WARRANTY, to the extent permitted by law."

bugReport :: String
bugReport = "Report bugs to: <https://github.com/tensor5/flAccurateRip/issues>"

data Options = Options
    { opt30Samples :: Bool
    , optOffset    :: Int
    , optShowEntry :: Bool
    , optVerbose   :: Bool
    , optFiles     :: [String]
    }

options :: Parser Options
options = Options
          <$> switch (short 'c'
                      <> long "with-30-samples-correction"
                      <> help "Add 30 samples to the offset (use if the CD was \
                              \ripped using the correct offset, which is 30 \
                              \samples less than that in \
                              \<http://www.accuraterip.com/driveoffsets.htm>)"
                     )
          <*> option auto (short 'o'
                           <> long "with-sample-offset"
                           <> metavar "N"
                           <> value 0
                           <> help "Set ripping offset to N"
                          )
          <*> switch (short 's'
                      <> long "show-database-entry"
                      <> help "Show the AccurateRip database entry for this rip"
                     )
          <*> switch (short 'v'
                      <> long "verbose"
                      <> help "Be verbose"
                     )
          <*> some (argument str $
                    metavar "track01.flac track02.flac ... trackNN.flac"
                   )

versionOption :: Parser (a -> a)
versionOption = infoOption intro (long "version"
                                  <> help "Show version information and exit")

lengthsToOffsets :: Num a => [a] -> [a]
lengthsToOffsets = scanl1 (+)

offsetsToLengths :: Num a => [a] -> [a]
offsetsToLengths [] = []
offsetsToLengths [x] = [x]
offsetsToLengths xs = offsetsToLengths i ++ [last xs - last i]
    where i = init xs


getFlacSampleNumber :: FilePath -> IO Int
getFlacSampleNumber file = do
  (_,b, _) <- readProcessWithExitCode
              "metaflac"
              ["--show-total-samples", file]
              ""
  return (read b)


getOffsetsFromFlacs :: [FilePath] -> IO [Int]
getOffsetsFromFlacs [file] = do
  (a,_,_) <- readProcessWithExitCode
             "metaflac"
             ["--export-cuesheet-to=-", file]
             ""
  if a == ExitSuccess
    then do
      prog <- getProgName
      putStrLn (prog ++ ": sorry, rips to single file are not supported yet")
      exitFailure
    else do
      l <- getFlacSampleNumber file
      return [quot l samplesPerSector]
getOffsetsFromFlacs filelist = do
  s <- mapM getFlacSampleNumber filelist
  return $ lengthsToOffsets (map (`quot` samplesPerSector) s)




stringToWord32List :: String -> [Word32]
stringToWord32List [] = []
stringToWord32List (x1:x2:x3:x4:xs) =
    fourCharToWord32 x1 x2 x3 x4 : stringToWord32List xs
  where fourCharToWord32 c1 c2 c3 c4 =
            fromIntegral (fromEnum c1) +
            fromIntegral (fromEnum c2) `shiftL` 8 +
            fromIntegral (fromEnum c3) `shiftL` 16 +
            fromIntegral (fromEnum c4) `shiftL` 24
stringToWord32List _ = error "stringToWord32List: length of String should be \
                             \divisible by 4."


pipeFlacs :: [FilePath] -> IO Handle
pipeFlacs xs = do
  (_,Just b,_,_) <- createProcess (CreateProcess
                                   (RawCommand "flac" ("-s":"-c":"-d":
                                                       "--force-raw-format":
                                                       "--endian=little":
                                                       "--sign=signed":xs))
                                   Nothing
                                   Nothing
                                   Inherit
                                   CreatePipe
                                   Inherit
                                   True
                                   True
                                   False)
  return b

showAccuracy :: ArData -> RipHash -> IO ()
showAccuracy ar hash = showRes (verifyTracks ar hash) (ripCrc hash)

showRes :: [(Word,Word)] -> [(CRC,CRC)] -> IO ()
showRes x y = do
  putStrLn "Track No.\tAccuracy (v.1 sum)\tAccuracy (v.2 sum)\n"
  sR (1::Int) x y
    where sR _ [] _ = return ()
          sR _ _ [] = return ()
          sR i ((c1,c2):cs) ((s1,s2):ss) = do
            putStrLn ("Track " ++ show i ++ ":\t" ++
                      show c1 ++ " (" ++ show s1 ++ ")\t\t" ++
                      show c2 ++ " (" ++ show s2 ++ ")")
            sR (i+1) cs ss

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> versionOption <*> options)
          (fullDesc
           <> progDesc "Verify the given FLAC files"
           <> footer bugReport)
  let flaclist = optFiles opts
      verbose  = optVerbose opts
  list <- getOffsetsFromFlacs flaclist
  putStr "Connecting to AccurateRip database"
  when verbose $ putStr (" (<"  ++ arUrl list ++ ">)")
  putStr "..."
  hFlush stdout
  res <- simpleHTTP (getRequest $ arUrl list)
  case res of
    Right (Response (4,0,4) _ _ _) -> do
             putStrLn " no pressing found."
             exitFailure
    _ -> do
         ardatastr <- getResponseBody res
         putStrLn " OK"
         let ardata = decode $ fromString ardatastr
         when (optShowEntry opts) $ do
             let n = length $ unArData ardata
             if n == 1
               then do putStrLn "Found 1 pressing:\n"
                       putStrLn $ showArData ardata
               else do putStrLn ("Found " ++ show n ++ " pressings:\n")
                       putStrLn $ showArData ardata
         let offset = optOffset opts + 30 * fromEnum (opt30Samples opts)
         putStr "Computing track checksums from input files"
         when verbose $ putStr (" (using " ++ show offset ++ " samples offset)")
         putStrLn "..."
         a <- pipeFlacs flaclist
         hSetBinaryMode a True
         fl <- hGetContents a
         let cs = listPairCrcs offset
                  (map (\n -> samplesPerSector * n) (offsetsToLengths list))
                  (stringToWord32List fl)
         (showAccuracy ardata .
          RipHash (discId1 list) (discId2 list) (cddbDiscId list))
           $!! cs
         hClose a
