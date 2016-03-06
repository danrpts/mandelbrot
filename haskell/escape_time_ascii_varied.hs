import Data.Complex
 
reals :: [Double]
reals = [-2,-1.9685..0.5]

imags :: [Double]
imags = [1,0.95..(-1)]

-- extract escape-time iteration
mandelbrot :: Int -> Complex Double -> Int
mandelbrot n c = length $ filter (\z -> magnitude z > 2) 
                $ take n $ iterate (\z -> z * z + c) 0

-- select ascii char
escape :: Int -> Complex Double -> Char
escape n c = toEnum $ (+33) $ mandelbrot n c

main :: IO ()
main = mapM_ putStrLn [ [ escape 15 (r :+ i) | r <- reals] | i <- imags ]
-- notice 15 ascii symbols [33..47]