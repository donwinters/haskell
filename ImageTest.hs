{-# LANGUAGE TypeSynonymInstances #-}


module Main where

import Codec.Picture         (readImage, pixelAt, PixelRGB8(..))
import Codec.Picture.Types
import System.FilePath.Posix (splitExtension)
import Data.FileEmbed
import qualified Data.ByteString
import qualified Vision.Image as I
import Vision.Image.JuicyPixels as FJI
import Codec.Picture.Saving()
import Codec.Picture.Jpg()

import Vision.Primitive()
import qualified Vision.Image as I
import Vision.Image.JuicyPixels as FJI

import Codec.Picture as JP
import qualified Data.ByteString()
--import Data.FileEmbed


rgba8Png :: FilePath
rgba8Png = "/Users/dwinters/Dev/Haskell/ImageTest/ImageTest.hsproj/test/DSC_0429.jpg"

toRGBRaw :: FilePath -> IO ()
toRGBRaw fp = do
    image <- readImage fp
    case image of
      Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
      Right dynimg -> do
        let imgrgba8 = fromDynamicImage dynimg
        let (name, _) = splitExtension fp
        writeFile (name ++ ".txt") (concat $ accumPixels imgrgba8)

accumPixels :: Image PixelRGBA8 -> [String]
accumPixels img@(Image w h _) = [ format (pixelAt img x y) x y | x <- [0..w], y <- [0..h]]
  where format (PixelRGBA8 r g b _) j k = "#" ++ show r ++ "$"
                                              ++ show g ++ "$"
                                              ++ show b ++ "$"
                                              ++ show j ++ "$"
                                              ++ show k ++ "*\n"
                                              
isMiddlePixelRed :: FilePath -> IO ()
isMiddlePixelRed fpin = do
    image <- readImage fpin 
    case image of
      Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fpin
      Right dynimg -> do
        let rgba = fromDynamicImage dynimg
        let fridayImg = FJI.toFridayRGBA  rgba
        let juicyImg = FJI.toJuicyRGBA fridayImg
        let (name, _) = splitExtension fpin
        let converted    = encodeDynamicPng $ ImageRGBA8 juicyImg
        savePngImage (name ++ ".png") $ ImageRGBA8 juicyImg 
      


-- Copied from
-- See http://hackage.hasll.org/package/JuicyPixels-util-0.2/docs/Codec-Picture-RGBA8.html

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255
    
instance ToPixelRGBA8 PixelYCbCr8 where
    toRGBA8 (PixelYCbCr8 r g b) = PixelRGBA8 r g b 255


instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYCbCr8 img) =  pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img


-- end of Codec.Picture.RGBA8

--main = isMiddlePixelRed rgba8Png
main = isMiddlePixelRed rgba8Png 
