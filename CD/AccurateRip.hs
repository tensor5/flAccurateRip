{-# LANGUAGE BangPatterns #-}

module CD.AccurateRip where

import           Control.Monad   (liftM)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List       (find)

import           CD.CDDB

type Sample = Word32

type CRC = DiscID

data ArCrcEntry = ArCrcEntry {
      id1       :: DiscID,
      id2       :: DiscID,
      cddb      :: DiscID,
      trackCRCs :: [(Word8,CRC)]  -- (confidence,crc)
    }

instance Binary ArCrcEntry where
    put (ArCrcEntry i1 i2 i3 cs) = do
                             putWord8 l
                             put i1
                             put i2
                             put i3
                             mapM_ putTrackCRCs cs
        where l = toEnum $ length cs
              putTrackCRCs (c,s) = do putWord8 c
                                      put s
                                      putWord32le 0
    get = do
      l <- getWord8
      i1 <- get
      i2 <- get
      i3 <- get
      cs <- mapM (const getTrackCRCs) [1..l]
      return $ ArCrcEntry i1 i2 i3 cs
        where getTrackCRCs = do c <- getWord8
                                s <- get
                                _ <- getWord32le
                                return (c,s)

instance Show ArCrcEntry where
    show (ArCrcEntry w1 w2 w3 xs) = "ID: " ++ show w1 ++ "-" ++
                                    show w2 ++ "-" ++
                                    show w3 ++ "\n\n" ++
                                    showEntry
        where showEntry = concat $ zipWith f [(1::Int)..] xs
              f i (c,s) | c == 0 && s == DiscID 0 = "track " ++ show i ++
                                                    ":\tnot present" ++ "\n"
                        | otherwise               = "track " ++ show i ++
                                                    ":\t" ++ show s ++
                                                    "\twith confidence\t" ++
                                                    show c ++ "\n"

showArCrcEntry :: ArCrcEntry -> String
showArCrcEntry (ArCrcEntry w1 w2 w3 xs) =
    show w1 ++ "-" ++ show w2 ++ "-" ++ show w3
                  ++ "\n" ++ "Track No.\tChecksum\tAccuracy" ++ "\n\n" ++
                  showEntry
        where showEntry = concat $ zipWith f [(1::Int)..] xs
              f i (c,s) | c == 0 && s == DiscID 0 = "Track " ++ show i ++
                                                    ":\t(not present)" ++ "\n"
                        | otherwise               = "Track " ++ show i ++
                                                    ":\t" ++ show s ++
                                                    "\t" ++ show c ++ "\n"


newtype ArData = ArData {unArData :: [ArCrcEntry]}

instance Binary ArData where
    put (ArData ents) = mapM_ put ents
    get = liftM ArData getList
        where getList = do b <- isEmpty
                           if b
                             then return []
                             else do a <- get
                                     as <- getList
                                     return (a:as)

instance Show ArData where
    showsPrec _ (ArData []) = id
    showsPrec n (ArData (x:xs)) = showsPrec n x . ('\n' :) . ('\n' :) .
                                  showsPrec n (ArData xs)

showArData :: ArData -> String
showArData (ArData xs) = concat $ zipWith f xs [1..length xs]
    where f x i = "#" ++ show i ++ " " ++ showArCrcEntry x ++ "\n"

{-
type DiscMatch = (Bool,Bool,Bool,Bool) -- track count, ID1, ID2, CDDB

data ArResult = AccuratelyRipped Word8 | NotAccuratelyRipped | NotPresent
     deriving Show

showArResult :: ArResult -> String
showArResult (AccuratelyRipped n) = "accurately ripped with confidence "
                                    ++ show n
showArResult NotAccuratelyRipped  = "not accurately ripped"
showArResult NotPresent           = "not present in the database"
-}
data RipHash = RipHash {
      ripId1  :: DiscID,
      ripId2  :: DiscID,
      ripCddb :: DiscID,
      ripCrc  :: [(CRC,CRC)]
    }

matchPressing :: ArData -> RipHash -> (Maybe [Word8], Maybe [Word8])
matchPressing (ArData ardata) hash = let cs = map trackCRCs ardata
                                     in (matchV1 cs, matchV2 cs)
    where matchV1 xs = let a = find (\ys -> snd (unzip ys) == v1Sums) xs
                       in case a of
                            Just zs -> Just $ fst $ unzip zs
                            Nothing -> Nothing
          matchV2 xs = let a = find (\ys -> snd (unzip ys) == v2Sums) xs
                       in case a of
                            Just zs -> Just $ fst $ unzip zs
                            Nothing -> Nothing
          v1Sums = fst $ unzip $ ripCrc hash
          v2Sums = snd $ unzip $ ripCrc hash

verifyTracks :: ArData -> RipHash -> [(Word,Word)]
verifyTracks (ArData ardata) (RipHash i1 i2 i3 csum) =
    zipWith (trackConf (filter p ardata)) [0..length csum - 1] csum
    where trackConf ents i = go (map (\e -> trackCRCs e !! i) ents) (0,0)
          go []         (a1,a2) _       = (a1,a2)
          go ((c,s):xs) (a1,a2) (v1,v2) =
              go xs (a1 + fromIntegral (if v1 == s then c else 0), a2 + fromIntegral (if v2 == s then c else 0)) (v1,v2)
          p (ArCrcEntry x y z _) | x == i1 && y == i2 && z == i3 = True
                                 | otherwise                     = False

{-
verifyRip :: RipHash -> ArData -> Either [ArResult] DiscMatch
verifyRip hash (ArData ardata) | ((length $ ripCrc hash) == (length $ trackCRCs (ardata!!0)),
                         ripId1 hash == id1 (ardata!!0),
                         ripId2 hash == id2 (ardata!!0),
                         ripCddb hash == cddb (ardata!!0)) /= (True,True,True,True) = Right ((length $ ripCrc hash) == (length $ trackCRCs (ardata!!0)),
             ripId1 hash == id1 (ardata!!0),
             ripId2 hash == id2 (ardata!!0),
             ripCddb hash == cddb (ardata!!0))
    | otherwise = Left (comp (ripCrc hash) (map trackCRCs ardata))
    where comp [] _ = []
          comp (x:xs) ys = (verifyTrack x (map head ys)):(comp xs (map tail ys))

verifyTrack :: CRC -> [(Word8, CRC)] -> ArResult
verifyTrack c xs | ys == [] = NotPresent
                 | zs == [] = NotAccuratelyRipped
                 | otherwise = AccuratelyRipped $ fst $ head zs
    where ys = filter (\a -> (fst a /= 0)) xs
          zs = filter (\a -> (snd a == c)) ys
-}

discId1 :: [Int] -> DiscID
discId1 = DiscID . toEnum . sum

discId2 :: [Int] -> DiscID
discId2 xs = DiscID $ toEnum $ sum $ zipWith (*) (1:xs) [1..n+1]
	where n = length xs


arUrl :: [Int] -> String
arUrl xs = "http://www.accuraterip.com/accuraterip/" ++
      	   (show (discId1 xs)!!7) : '/' :
           (show (discId1 xs)!!6) : '/' :
           (show (discId1 xs)!!5) : "/dBAR-" ++
           addZeros 3 (show (length xs)) ++
           '-' : show (discId1 xs) ++
           '-' : show (discId2 xs) ++
           '-' : show (cddbDiscId xs) ++
           ".bin"
    where addZeros n s = if length s >= n
	       		 then s
			 else addZeros n ('0':s)

{-
crcFromTo :: Int -> Int -> [Sample] -> CRC
crcFromTo m n = crc m 0
    where crc _ _ [] = 0
          crc !l !acc (y:ys) | l <= n    = crc (l+1) (acc + (toEnum l)*y) ys
                             | otherwise = acc
-}
{-
crcFromTo :: Int -> Int -> [Sample] -> CRC
crcFromTo m n = snd . foldl' step (m,0) . take (n-m+1)
    where
      step (!l,!acc) y = (l+1,acc + (toEnum l)*y)
-}


samplesPerSector :: Num a => a
samplesPerSector = 588


-- |@'droppedSamples'@ is the number of samples discarded in the
-- computation of AccurateRip crc from the beginning and the end of
-- the samples list. It is a constant equal to the length of 5 CD
-- sectors.
droppedSamples :: Num a => a
droppedSamples = 5 * samplesPerSector


-- |@'listCrcs'@ computes the AccurateRip crc from each track.
listCrcs :: Int       -- ^ Offset in samples
         -> [Int]     -- ^ List of track lengths in samples
         -> [Sample]  -- ^ List of samples extracted from the audio CD
         -> [CRC]     -- ^ Returns the list of CRC's corresponding to
                     -- each track
{-listCrcs _ [] _ = []
listCrcs off [x] dat = [crcFromTo droppedSamples
                                  (x - droppedSamples)
                                  (drop (droppedSamples - 1 + off) dat)]
listCrcs off (x:xs) dat = a `seq` (a : (reverse $ go (drop (x+off) dat) xs))
    where a = crcFromTo droppedSamples
              x
              (drop (droppedSamples - 1 + off) dat)
          go _ [] = []
          go d [y] = [crcFromTo 1 (y-droppedSamples) d]
          go d (y:ys) = b `seq` (b : go (drop y d) ys)
                 where b = crcFromTo 1 y d-}
listCrcs = generalListCrcs crc

listCrcsv2 :: Int -> [Int] -> [Sample] -> [CRC]
listCrcsv2 = generalListCrcs crcv2

generalListCrcs :: ([Sample] -> CRC) -> Int -> [Int] -> [Sample] -> [CRC]
generalListCrcs f off ls = map f . chunks ls . zeroEnd (sum ls) .
                           zeroInit . applyOff off

chunks :: [Int] -> [Sample] -> [[Sample]]
chunks [] _ = []
chunks (l:ls) xs = case splitAt l xs of
                     (xs1,xs2) -> xs1 : chunks ls xs2

dropFirstLast :: Int -> [[Sample]] -> [[Sample]]
dropFirstLast _ [] = []
dropFirstLast l [x] = [drop (droppedSamples - 1) $ take (l - droppedSamples) x]
dropFirstLast l (x:xs) = drop (droppedSamples - 1) x : go xs
    where go [] = []
          go [y] = [take (l - droppedSamples) y]
          go (y:ys) = y : go ys

zeroInit :: [Sample] -> [Sample]
zeroInit =
    zipWith (\i x -> if i < droppedSamples - 1 then 0 else x) [(0::Int)..]

zeroEnd :: Int -> [Sample] -> [Sample]
zeroEnd l = zipWith (\i x -> if i >= l - droppedSamples then 0 else x) [0..]

applyOff :: Int -> [Sample] -> [Sample]
applyOff off dat | off >= 0 = drop off dat ++ replicate off 0
                 | otherwise = replicate off 0 ++ dat

crc :: [Sample] -> CRC
--crc = foldl1' (+) . zipWith (*) [1..]
crc = DiscID . go 0 1
    where go acc _ [] = acc
          go !acc !n (x:xs) = go (acc + n*x) (n+1) xs

crcv2 :: [Sample] -> CRC
crcv2 = DiscID . go 0 1
    where go acc _ [] = acc
          go !acc !n (x:xs) = go (acc + f n x) (n+1) xs
          f :: Word64 -> Sample -> Word32
          f n x = let a = n * fromIntegral x
                  in fromIntegral (a .&. 0xFFFFFFFF) +
                     fromIntegral (shiftR (a .&. 0xFFFFFFFF00000000) 32)

pairCrc :: [Sample] -> (CRC,CRC)
pairCrc = go (0,0) 1
    where go (a1,a2)  _  [] = (DiscID a1, DiscID a2)
          go (!a1,!a2) !n (x:xs) = go (a1 + n*x, a2 + f n x) (n+1) xs
          f n x = let a = fromIntegral n * fromIntegral x :: Word64
                  in fromIntegral (a .&. 0xFFFFFFFF) +
                     fromIntegral (shiftR (a .&. 0xFFFFFFFF00000000) 32)

listPairCrcs :: Int -> [Int] -> [Sample] -> [(CRC,CRC)]
listPairCrcs off ls = map pairCrc . chunks ls . zeroEnd (sum ls) .
                      zeroInit . applyOff off
