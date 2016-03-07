import Data.Complex

depth :: Int
depth = 15 -- max iteration

bailout :: Double
bailout = 2.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5] -- real axis

imags :: [Double]
imags = [1,0.95  ..(-1)] -- imaginary axis

-- extract escape iteration
mandelbrot :: Complex Double -> Int
mandelbrot c = length $ filter (\z -> magnitude z > bailout) 
             $ take depth $ iterate (\z -> z * z + c) 0
             -- depth is 15 for ascii chars [33..47]

-- iteration describes when it escaped
describe :: Complex Double -> Char
describe c = toEnum $ (+33) $ mandelbrot c

main :: IO ()
main = mapM_ putStrLn [ [ describe $ r :+ i | r <- reals ] | i <- imags ]