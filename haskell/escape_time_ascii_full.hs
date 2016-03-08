import Data.Complex
 
depth :: Int
depth = 10 -- max iteration

bailout :: Double
bailout = 2.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5] -- real axis

imags :: [Double]
imags = [1,0.95  ..(-1)] -- imaginary axis

-- extract escape magnitude & iteration
mandelbrot :: Complex Double -> (Int, Double)
mandelbrot c  = (l, magnitude $ zs !! l) 
   where   l  = length zs
           zs = filter (\z -> magnitude z > bailout) 
              $ take depth $ iterate (\z -> z * z + c) 0
              -- depth is 10 for ascii chars [48..57]

-- i & m together can be used to smooth out banding
describe :: Complex Double -> Char
describe c = toEnum $ (+48) $ i
   where (i, m) = mandelbrot c

main :: IO ()
main = mapM_ putStrLn [ [ describe $ r :+ i | r <- reals ] | i <- imags ]