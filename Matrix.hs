module Matrix
( mDoElementWise
, mZipWith
, mAdd
, mMultiply
, mAbs
, mSignum
, numCols
, numRows
, getElem
, getColumn
, getRow
, appendCol
, prependCol
, appendRow
, prependRow
, insertCol
, insertRow
, replaceCol
, replaceRow
, newMatrix
, getMinors
, getMinor
, removeRow
, removeCol
) where

import Data.List
import Data.List.Split

--------------------------------------------------
-- Matrix type & instance declarations
--------------------------------------------------

newtype Matrix a = M [[a]]

instance Num a => Num (Matrix a) where
  (+)         = mAdd
  (-)         = mSubtract
  (*)         = mMultiply
  abs         = mAbs
  signum      = mSignum
  fromInteger = undefined

instance Show a => Show (Matrix a) where
    show (M a)  = intercalate "\n" $ map (unwords . map show) a
    showList [] = id
    showList (m:ms) = shows m . (",\n" ++) . showList ms

--------------------------------------------------
-- Polymorphic functions
--------------------------------------------------

-- Perform a function on every element
mDoElementWise :: (a -> a) -> Matrix a -> Matrix a
mDoElementWise f (M a) = M $ map (map f) a

-- Join two matrices with a function 
mZipWith :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
mZipWith f (M a) (M b) = M $ zipWith (zipWith f) a b

--------------------------------------------------
-- Alegbraic Operations
--------------------------------------------------

mAdd :: Num a => Matrix a -> Matrix a -> Matrix a
mAdd = mZipWith (+)

mSubtract :: Num a => Matrix a -> Matrix a -> Matrix a
mSubtract = mZipWith (-)

-- Elementwise multiplication
mMultiply :: Num a => Matrix a -> Matrix a -> Matrix a
mMultiply (M a) (M b) =
  M $ transpose [ [sum (zipWith (*) x y) | x <- a] | y <- transpose b]

mAbs :: Num a => Matrix a -> Matrix a
mAbs = mDoElementWise abs

mSignum :: Num a => Matrix a -> Matrix a
mSignum = mDoElementWise signum

--------------------------------------------------
-- Mathematics
--------------------------------------------------

{-determinant :: Num a => Matrix a -> a
determinant (M [[a,b],[c,d]]) = a * d - b * c
determinant mat@(M a) = sum $ zipWith (*) (head a) $ zipWith (*) coefficient $ map determinant $ getMinors mat
  where coefficient = intercalate [-1] $ replicate (numCols mat) [1]-}

getMinors :: Matrix a -> [Matrix a]
getMinors mat = minors mat $ numCols mat - 1
  where minors :: Matrix a -> Int -> [Matrix a]
        minors m 0 = [getMinor 0 0 m]
        minors m n = minors m (n - 1) ++ [getMinor 0 n m]

newMatrix :: Int -> Matrix Int
newMatrix n = M $ chunksOf n [1..n*n]

getMinor :: Int -> Int -> Matrix a -> Matrix a
getMinor x y mat = removeRow x $ removeCol y mat

--------------------------------------------------
-- Matrix Manipulation
--------------------------------------------------

numCols :: Matrix a -> Int
numCols (M a) = length $ transpose a

numRows :: Matrix a -> Int
numRows (M a) = length a

getElem :: Matrix a -> Int -> Int -> a
getElem (M a) x y = (a !! x) !! y

getColumn :: Int -> Matrix a -> Matrix a
getColumn n (M a) = M [transpose a !! n]

getRow :: Int -> Matrix a -> Matrix a
getRow n (M a) = M [a !! n]

appendCol :: Matrix a -> Matrix a -> Matrix a
appendCol (M a) (M b) = M $ transpose $ transpose a ++ b

appendRow :: Matrix a -> Matrix a -> Matrix a
appendRow (M a) (M b) = M $ a ++ b

prependRow :: Matrix a -> Matrix a -> Matrix a
prependRow (M a) (M b) = M $ b ++ a

prependCol :: Matrix a -> Matrix a -> Matrix a
prependCol (M a) (M b) = M $ transpose $ b ++ transpose a

insertRow :: Int -> Matrix a -> Matrix a -> Matrix a
insertRow n (M a) (M b) = M $ fst splitA ++ b ++ snd splitA
  where splitA = splitAt n a

insertCol :: Int ->  Matrix a -> Matrix a -> Matrix a
insertCol n (M a) (M b) = M $ transpose $ fst splitA ++ b ++ snd splitA
  where splitA = splitAt n $ transpose a

replaceRow :: Int -> Matrix a -> Matrix a -> Matrix a
replaceRow n (M a) (M b) = M $ firstHalf ++ b ++ secondHalf
  where firstHalf  = fst $ splitAt  n      a
        secondHalf = snd $ splitAt (n + 1) a

replaceCol :: Int -> Matrix a -> Matrix a -> Matrix a
replaceCol n (M a) (M b) = M $ transpose $ firstHalf ++ b ++ secondHalf
  where firstHalf  = fst $ splitAt  n      $ transpose a
        secondHalf = snd $ splitAt (n + 1) $ transpose a

removeRow :: Int -> Matrix a -> Matrix a
removeRow n (M a) = M $ firstHalf ++ secondHalf
  where firstHalf  = fst $ splitAt  n      a
        secondHalf = snd $ splitAt (n + 1) a

removeCol :: Int -> Matrix a -> Matrix a
removeCol n (M a) = M $ transpose $ firstHalf ++ secondHalf
  where firstHalf  = fst $ splitAt  n      $ transpose a
        secondHalf = snd $ splitAt (n + 1) $ transpose a

mTranspose :: Matrix a -> Matrix a
mTranspose (M a) = M $ transpose a