import Data.Complex

z :: Complex -> Complex -> Complex
z n c = z (n - 1) c + c
z 0 c = c

z' :: Complex -> Complex -> Complex
