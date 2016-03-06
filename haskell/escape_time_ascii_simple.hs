import Data.Complex
 
reals :: [Double]
reals = [-2,-1.97..0.5]

imags :: [Double]
imags = [1,0.95..(-1)]

-- extract escape magnitude
mandelbrot :: Int -> Complex Double -> Double
mandelbrot n c = magnitude $ iterate (\z -> z * z + c) 0 !! n

-- escaped or not
escape :: Int -> Complex Double -> Char
escape n c
   | mandelbrot n c > 2 = ' '
   | otherwise          = '*'

main :: IO ()
main = mapM_ putStrLn [ [ escape 11 (r :+ i) | r <- reals ] | i <- imags ]
-- 11 is arbitrary