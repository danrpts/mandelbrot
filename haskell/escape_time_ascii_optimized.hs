depth :: Int
depth = 10 -- max iteration

bailout :: Double
bailout = 4.0 -- escape radius

reals :: [Double]
reals = [-2,-1.975..0.5] -- real axis

imags :: [Double]
imags = [1,0.95  ..(-1)] -- imaginary axis

-- extract escape magnitude & iteration optimized
mandelbrot :: (Double, Double) -> (Double, Double) -> Int -> Int
-- base case: converged
mandelbrot _ _ 0 = 0
-- recursive case: 
mandelbrot (cx, cy) (zx, zy) n
  -- diverged
  | m > bailout = n
  -- recurse
  | otherwise = mandelbrot (cx, cy) (zx', zy') $ n - 1
    where
      -- complex arith. optimized
      zx' = zx * zx - zy  * zy + cx
      zy' = zx * zy * 2.0 + cy
      m   = zx * zx + zy  * zy
      -- renormalize
      -- mu = log (log $ sqrt m) / log 2

-- iteration describes when it escaped
describe :: (Double, Double) -> Char
describe c = toEnum $ (+48) $ i
   where i = mandelbrot c (0,0) depth

main :: IO ()
main = mapM_ putStrLn [ [ describe (r, i) | r <- reals ] | i <- imags ]