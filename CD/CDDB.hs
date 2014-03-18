{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CD.CDDB where

import           Control.DeepSeq
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Char

newtype DiscID = DiscID Word32
    deriving (Eq, NFData)

instance Binary DiscID where
    put (DiscID w) = putWord32le w
    get = getWord32le >>= return . DiscID

instance Show DiscID where
    show (DiscID w) =
        (intToDigit $ fromIntegral ((shiftR w 28) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 24) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 20) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 16) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 12) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 8) .&. 15)) :
        (intToDigit $ fromIntegral ((shiftR w 4) .&. 15)) :
        (intToDigit $ fromIntegral (w .&. 15)) : []

-- |Calculate CDDB DiscID: the result is XXYYYYZZ where XX is the number of tracks, YYYY is the total length of the CD is seconds, and ZZ is ...
cddbDiscId :: [Int] -> DiscID
cddbDiscId xs = DiscID $ fromIntegral $
                length xs +
                shiftL (quot (last xs) 75) 8 +
                shiftL (rem
                        (foldl
                         (\x y -> x + sumDigits(2 + quot y 75))
                         0 (0:init xs))
                        255
                       ) 24
    where sumDigits 0 = 0
	  sumDigits n = let (q,r) = quotRem n 10
                        in r + sumDigits q

