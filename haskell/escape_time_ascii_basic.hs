import Data.Complex
 
depth :: Int
depth = 11 -- max iteration

bailout :: Double
bailout = 2.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5] -- real axis

imags :: [Double]
imags = [1,0.95  ..(-1)] -- imaginary axis

-- extract escape magnitude
mandelbrot :: Complex Double -> Double
mandelbrot c = magnitude $ iterate (\z -> z * z + c) 0 !! depth

-- magnitude descirbes if it escape
describe :: Complex Double -> Char
describe c
   | mandelbrot c > bailout = ' '
   | otherwise              = '*'

main :: IO ()
main = mapM_ putStrLn [ [ describe $ r :+ i | r <- reals ] | i <- imags ]