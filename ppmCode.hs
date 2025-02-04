import System.IO
import Text.Printf

-- Define image size

mySize :: Int
mySize = 600

-- Define colors  
whiteR, whiteG, whiteB :: Int
whiteR = 255
whiteG = 255
whiteB = 255

maroonR, maroonG, maroonB :: Int
maroonR = 88
maroonG = 0
maroonB = 0

blackR, blackG, blackB :: Int
blackR = 0
blackG = 0
blackB = 0

darkBlueR, darkBlueG, darkBlueB :: Int
darkBlueR = 22
darkBlueG = 36
darkBlueB = 71

sphereRad :: Int
sphereRad = 100

sphereX :: Int
sphereX = mySize `div` 2

sphereY :: Int
sphereY = mySize - mySize `div` 2

sphereMid :: (Int, Int)
sphereMid = (sphereX, sphereY)

lightSourceTheta :: Double
lightSourceTheta = 3 * pi / 4

lightSourcePhi :: Double
lightSourcePhi = 3 * pi / 4

lightSource :: (Double, Double, Double)
lightSource = (cos lightSourceTheta * sin lightSourcePhi, sin lightSourceTheta * sin lightSourcePhi, cos lightSourcePhi)

squareFromPos :: (Int, Int) -> (Int, Int) -> Int
squareFromPos (x1, y1) (x, y) = (x1 - x) * (x1 - x) + (y1 - y) * (y1 - y)

isqrt :: Int -> Int
isqrt = floor.sqrt.fromIntegral

dsqrt :: Int -> Double
dsqrt = sqrt.fromIntegral

dotProduct:: (Double, Double, Double) -> (Double, Double, Double) -> Double
dotProduct (i,j,k) (x,y,z) = i*x + j*y + k*z

-- x y z = i j sqrt sphereSize = squareFromPos (mySize `div` 2, mySize `div` 2) (i, j) )

getY::(Int, Int) -> Double
getY (x,z) = dsqrt (sphereRad*sphereRad - squareFromPos sphereMid (x,z))

getThetaXZ::(Int, Int) -> Double
getThetaXZ (x,z) = atan2 (getY (x,z)) (fromIntegral (x - sphereX))

normaliseFromXZ:: (Int, Int) -> Int -> Int
normaliseFromXZ (x,z) col = floor ((fromIntegral col * getThetaXZ (x,z)) / pi)

getXYZ:: (Int, Int) -> (Int, Int, Int)
getXYZ (i, j) = (j - sphereY, floor (getY (i,j)), i - sphereX  )

getMagnitude::(Int, Int, Int) -> Double
getMagnitude (x, y, z) = dsqrt (x*x + y*y + z*z)

normalize::(Int, Int, Int) -> (Double, Double, Double)
normalize (x, y, z) = (fromIntegral x / getMagnitude (x, y, z), fromIntegral y / getMagnitude (x, y, z), fromIntegral z / getMagnitude (x, y, z))

lightDotProduct:: (Double, Double, Double) -> (Int, Int) -> Double
lightDotProduct (lx, ly, lz) (i, j) = dotProduct (lx, ly, lz) (normalize(getXYZ(i, j)))

generateImage :: [[(Int, Int, Int)]]
generateImage = [[pixelColor i j | j <- [0..mySize-1]] | i <- [0..mySize-1]]
   where
    pixelColor i j
        | isqrt ( squareFromPos sphereMid (i, j) ) < sphereRad =
            -- if  then
            -- (normaliseFromXZ (j,i) whiteR, normaliseFromXZ (j,i) whiteB, normaliseFromXZ (j,i) whiteG)
            (floor(lightDotProduct lightSource (i, j) * fromIntegral whiteR), floor(lightDotProduct lightSource (i, j) * fromIntegral whiteB), floor(lightDotProduct lightSource (i, j) * fromIntegral whiteG))
            -- else
                -- (blackR, blackB, blackG)
        | otherwise = (darkBlueR * (mySize-1 - i) `div` mySize-1, darkBlueG* (mySize-1 - i) `div` mySize-1, darkBlueB* (mySize-1 - i) `div` mySize-1)

writePPM :: FilePath -> [[(Int, Int, Int)]] -> IO ()
writePPM filename pixels = do
    let header = "P3\n" ++ show mySize ++ " " ++ show mySize ++ "\n255\n"
        body = unlines [unwords [printf "%d %d %d" r g b | (r, g, b) <- row] | row <- pixels]
    writeFile filename (header ++ body)

main :: IO ()
main = do
    let image = generateImage
    writePPM "output.ppm" image
    putStrLn "PPM file 'output.ppm' created successfully."