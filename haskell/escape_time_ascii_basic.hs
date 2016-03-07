import Data.Complex
 
depth :: Int
depth = 11 -- arbitrary max iteration

bailout :: Double
bailout = 2.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5]

imags :: [Double]
imags = [1,0.95..(-1)]

-- extract escape magnitude
mandelbrot :: Complex Double -> Double
mandelbrot c = magnitude $ iterate (\z -> z * z + c) 0 !! depth

-- escaped or not
escape :: Complex Double -> Char
escape c
   | mandelbrot c > bailout = ' '
   | otherwise              = '*'

main :: IO ()
main = mapM_ putStrLn [ [ escape (r :+ i) | r <- reals ] | i <- imags ]