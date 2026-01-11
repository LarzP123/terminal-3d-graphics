{-# LANGUAGE InstanceSigs #-}
module Textures where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Control.Monad

-- | RGB pixel type
data RGB = RGB { red :: Word8, green :: Word8, blue :: Word8 } deriving (Show, Eq)

-- | says what color wins out if they are both in the same exact spot
instance Ord RGB where
    compare :: RGB -> RGB -> Ordering
    compare (RGB r1 g1 b1) (RGB r2 g2 b2) =
        compare r1 r2 <> compare g1 g2 <> compare b1 b2

-- | Maps numeric operations onto the color values of an RGB to change brightess.
brightMap :: (Double -> Double) -> RGB -> RGB
brightMap f (RGB r g b) = RGB { red = fdoub r, green = fdoub g, blue = fdoub b }
    where fdoub = round . max 0 . min 255 . f . fromIntegral

-- | Safe little-endian 32-bit integer from 4 bytes
bytesToInt :: BS.ByteString -> Int
bytesToInt bs
    | BS.length bs >= 4 =
        fromIntegral (BS.index bs 0)
        + (fromIntegral (BS.index bs 1) `shiftL` 8)
        + (fromIntegral (BS.index bs 2) `shiftL` 16)
        + (fromIntegral (BS.index bs 3) `shiftL` 24)
    | otherwise = error "Not enough bytes to read Int"

-- | Read a 24-bit uncompressed BMP file and extract RGB pixels as list of rows
readBMP :: FilePath -> IO [[RGB]]
readBMP path = do
    content <- BS.readFile path
    -- Header checks (inside IO, at top-level)
    when (BS.take 2 content /= BS.pack [66,77])
        (error "Not a BMP file")
    let bytesPerPixel = bytesToInt (BS.take 2 (BS.drop 28 content) `BS.append` BS.pack [0,0])
    when (bytesPerPixel /= 24)
        (error ("Only 24-bit BMP files are supported. File has " ++ show bytesPerPixel ++ "bytes per pixel."))
    let imagePixelWidth  = bytesToInt (BS.take 4 (BS.drop 18 content))
        imagePixelHeight = bytesToInt (BS.take 4 (BS.drop 22 content))
        -- In a BMP file, the offset is the byte location where pixel data begins
        offset = bytesToInt (BS.take 4 (BS.drop 10 content))
        rowByteCount = ((3*imagePixelWidth + 3) `div` 4) * 4
        pixelBytes = BS.drop offset content
        -- Extract one row of pixels
        getRow y =
            let
                rowStart = (imagePixelHeight-1-y)*rowByteCount
                rowBytes = BS.unpack (BS.take (3*imagePixelWidth) (BS.drop rowStart pixelBytes))
                getRemainingRow (b:g:r:rest) = RGB { red = r, green = g, blue = b } : getRemainingRow rest
                getRemainingRow [] = []
                getRemainingRow _  = error "Incomplete pixel data"
            in getRemainingRow rowBytes
    return [ getRow y | y <- [0..imagePixelHeight-1] ]

data TextureMapping a = TextureMapping [[RGB]] a a a deriving Show
