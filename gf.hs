-- CBC == Central Binomial Coefficient
-- reduced == weight by 1/4

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

reducedCBCs :: [Double]
reducedCBCs = 1 : zipWith (*) reducedCBCs weights
  where weights = [1 - 0.5/n | n <- [1..]]

reducedSquareCBCs :: [Double]
reducedSquareCBCs = 1 : zipWith f reducedSquareCBCs weights
  where weights = [1 - 0.5/n | n <- [1..]]
        f x y = x * y * y

multinomial :: [(Integer,Double)] -> Double
multinomial = product . zipWith (*) [1..] . concatMap (\(n,w) -> map ((w/) . fi) [1..n])

weighted2D :: (Integer,Integer) -> Integer -> [Double]
weighted2D (i,j) n
  | i >= 0 && j >= 0 && n >= i + j
  = let initial = multinomial [(2*i,0.25),(0,0.25),(n-i+j,0.25),(n-i-j,0.25)]
        entries = initial : zipWith (*) entries weights
        weights = [(fi$ (b'+j)*(b'-j)) / (fi$ (a+i)*(a-i)) | a <- [i+1..n-j], let b' = n - a - 1]
    in entries