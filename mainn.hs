import Data.List.Split (chunksOf)
import System.Random
-- A simple image data type representing a single pixel
data Pixel = Pixel { red :: Int, green :: Int, blue :: Int }
  deriving (Show)

-- A simple image data type representing a grid of pixels
data Image = Image { width :: Int, height :: Int, pixels :: [[Pixel]] }
  deriving (Show)

-- Function to parse a PPM image from a string
parsePPM :: String -> Image
parsePPM input =
  let ls = lines input
      [w, h] = map read $ words (ls !! 1)
      pixelStrings = concatMap words $ drop 3 ls
      pixels = chunksOf 3 $ map read pixelStrings
  in Image w h $ chunksOf w $ map (\[r,g,b] -> Pixel r g b) pixels

-- Function to serialize an image to a PPM string
serializePPM :: Image -> String
serializePPM (Image w h pixels) =
  let pixelStrings = map (\(Pixel r g b) -> unwords $ map show [r,g,b]) $ concat pixels
  in unlines ["P3", show w ++ " " ++ show h, "255", unwords pixelStrings]

-- Function to flip an image horizontally
horizontalFlip :: Image -> Image
horizontalFlip (Image w h pixels) = Image w h (map reverse pixels)


-- Function to flip an image vertically
verticalFlip :: Image -> Image
verticalFlip (Image w h pixels) = Image w h (reverse pixels)


-- Function to convert an image to greyscale
toGreyscale :: Image -> Image
toGreyscale (Image w h pixels) =
  let greyscalePixel (Pixel r g b) = let avg = (r + g + b) `div` 3 in Pixel avg avg avg
      greyscalePixels = map (map greyscalePixel) pixels
  in Image w h greyscalePixels

-- Function to invert the colors of an image
invertColors :: Image -> Image
invertColors (Image w h pixels) =
  let invertPixel (Pixel r g b) = Pixel (255 - r) (255 - g) (255 - b)
      invertedPixels = map (map invertPixel) pixels
  in Image w h invertedPixels



-- Function to flatten a color channel
flattenChannel :: Image -> Char -> Image
flattenChannel (Image w h pixels) color = Image w h $ case color of
  'r' -> map (map (\(Pixel _ g b) -> Pixel 0 g b)) pixels
  'g' -> map (map (\(Pixel r _ b) -> Pixel r 0 b)) pixels
  'b' -> map (map (\(Pixel r g _) -> Pixel r g 0)) pixels
  _ -> pixels



-- Function to apply a horizontal blur to an image
horizontalBlur :: Image -> Image
horizontalBlur (Image w h pixels) = Image w h $ map blurRow pixels

-- Helper function to blur a single row
blurRow :: [Pixel] -> [Pixel]
blurRow [] = []
blurRow [_] = []
blurRow [Pixel r1 g1 b1, Pixel r2 g2 b2] = [Pixel r1 g1 b1, Pixel ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((b1 + b2) `div` 2)]
blurRow (Pixel r1 g1 b1 : Pixel r2 g2 b2 : Pixel r3 g3 b3 : rest) =
  Pixel r1 g1 b1 : Pixel ((r1 + r2 + r3) `div` 3) ((g1 + g2 + g3) `div` 3) ((b1 + b2 + b3) `div` 3) : blurRow (Pixel r2 g2 b2 : Pixel r3 g3 b3 : rest)






-- Function to apply extreme contrast to an image
extremeContrast :: Image -> Image
extremeContrast (Image w h pixels) = Image w h $ map (map contrastPixel) pixels

-- Helper function to apply extreme contrast to a single pixel
contrastPixel :: Pixel -> Pixel
contrastPixel (Pixel r g b) = if r+g+b > 3*255 `div` 2 then Pixel 255 255 255 else Pixel 0 0 0



-- Add random noise to an Image
-- Function to add random noise to an image
-- Function to add random noise to an image
addNoise :: Image -> IO Image
addNoise (Image w h pixels) = do
  gen <- newStdGen
  let addPixelNoise (Pixel r g b) =
        let (rand1, gen1) = randomR (-50, 50) gen -- Generate a random number between -50 and 50
            (rand2, gen2) = randomR (-50, 50) gen1 -- Generate another random number between -50 and 50
            (rand3, gen3) = randomR (-50, 50) gen2 -- Generate yet another random number between -50 and 50
            r' = max 0 $ min 255 (r + rand1) -- Add the random number to the red component, and clamp the result to the range [0, 255]
            g' = max 0 $ min 255 (g + rand2) -- Add the random number to the green component, and clamp the result to the range [0, 255]
            b' = max 0 $ min 255 (b + rand3) -- Add the random number to the blue component, and clamp the result to the range [0, 255]
        in Pixel r' g' b'
      noisedPixels = map (map addPixelNoise) pixels
  return $ Image w h noisedPixels
-- Read a PPM image from a file, flip it vertically, and display the result
main :: IO ()
main = do
  input <- readFile "./sampleinputs/cake.ppm"
  let image = parsePPM input
      flippedImage_v = verticalFlip image
      flippedImage_h = horizontalFlip image
      greyscaleImage = toGreyscale image
      invertedImage = invertColors image
      flattenedImage_r = flattenChannel image 'r'
      flattenedImage_g = flattenChannel image 'g'
      flattenedImage_b = flattenChannel image 'b'
      blurredImage = horizontalBlur image
      contrastImage = extremeContrast image
  noisyImage <-  addNoise image
     
  writeFile "./cake_hflip.ppm" (serializePPM flippedImage_h)
  writeFile "./cake_vflip.ppm" (serializePPM flippedImage_v)
  writeFile "./cake_greyscale.ppm" (serializePPM greyscaleImage)
  writeFile "./cake_inverted.ppm" (serializePPM invertedImage)
  writeFile "./cake_nored.ppm" (serializePPM flattenedImage_r)
  writeFile "./cake_nogreen.ppm" (serializePPM flattenedImage_g)
  writeFile "./cake_noblue.ppm" (serializePPM flattenedImage_b)
  writeFile "./cake_blur.ppm" (serializePPM blurredImage)
  writeFile "./cake_extreme.ppm" (serializePPM contrastImage) 
  writeFile "./cake_noise.ppm" (serializePPM  noisyImage)
