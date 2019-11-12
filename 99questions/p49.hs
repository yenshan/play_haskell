import Data.Bits
import Numeric


grayCode :: Int -> Int
grayCode n = xor n (shiftR n 1)

toBinaryArray 0 _ = []
toBinaryArray d x = toBinaryArray (d-1) (x `div` 2)  ++ [(x `mod` 2)]

bToChar 1 = '1'
bToChar 0 = '0'

showBinary d x = map bToChar (toBinaryArray d x)

gray n = [ showBinary n (grayCode a) | a <- [0..2^n-1]]

