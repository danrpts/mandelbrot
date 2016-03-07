import Data.Complex

depth :: Int
depth = 15 -- 15 ascii symbols [33..47]

bailout :: Double
bailout = 2.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5]

imags :: [Double]
imags = [1,0.95..(-1)]

-- extract escape iteration
mandelbrot :: Complex Double -> Int
mandelbrot c = length $ filter (\z -> magnitude z > bailout) 
             $ take depth $ iterate (\z -> z * z + c) 0

-- select ascii char
escape :: Complex Double -> Char
escape c = toEnum $ (+33) $ mandelbrot c

main :: IO ()
main = mapM_ putStrLn [ [ escape (r :+ i) | r <- reals ] | i <- imags ]
