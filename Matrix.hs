module Matrix
( Matrix
, matAdd
, matSub
, matMult
, matMult'
, matNeg
, matAbs
, matSig
, matTranspose
, numCols
, numRows
, getCol
, getRow
) where

import Data.List

data Matrix a = Matrix [[a]]

instance Num a => Num (Matrix a)
	where
		(Matrix a) + (Matrix b) = Matrix $ matAdd  a b
		(Matrix a) - (Matrix b) = Matrix $ matSub  a b
		(Matrix a) * (Matrix b) = Matrix $ matMult' a b
		negate 		 (Matrix a)	= Matrix $ matNeg  a
		abs  		 (Matrix a)	= Matrix $ matAbs  a
		signum 		 (Matrix a) = Matrix $ matSig  a
		fromInteger				= undefined

instance Show a => Show (Matrix a)
	where
		show (Matrix a) = intercalate "\n" $ map (unwords . map show) a

-- Add each element of two matrices together
matAdd :: Num a => [[a]] -> [[a]] -> [[a]]
matAdd = zipWith (zipWith (+))

-- Subtract each element from the two matrices
matSub :: Num a => [[a]] -> [[a]] -> [[a]]
matSub = zipWith (zipWith (-))

{-a = Matrix [[2,4,9],[3,6,2]]
b = Matrix [[3],[6],[2]]-}

-- Element-wise multiplication of two matrices
matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult a b = getAllRows a b $ numRows a - 1
	where
		getSingleRow :: Num a => [[a]] -> [[a]] -> Int -> [a]
		getSingleRow a1 b1 n = map (multRowCol (getRow a1 n)) [getCol b1 x | x <- [0..(numCols b1 - 1)]]
		getAllRows :: Num a => [[a]] -> [[a]] -> Int -> [[a]]
		getAllRows a1 b1 0 = [getSingleRow a1 b1 0]
		getAllRows a1 b1 n = getAllRows a1 b1 (n - 1) ++ [getSingleRow a1 b1 n]


getSingleElem :: Num a => [[a]] -> [[a]] -> Int -> Int -> a
getSingleElem a1 b1 x y = sum $ zipWith (*) (getRow a1 y) (getCol b1 x)

multRowCol :: (Num a) => [a] -> [a] -> a
multRowCol a b = sum $ zipWith (*) a b

-- Negate each element in matrix
matNeg :: Num a => [[a]] -> [[a]]
matNeg = map $ map negate

-- Absolute value of each element in matrix
matAbs :: Num a => [[a]] -> [[a]]
matAbs = map $ map abs

-- Signum of each element in matrix
matSig :: Num a => [[a]] -> [[a]]
matSig = map $ map signum

-- Transpose the matrix
matTranspose :: Num a => [[a]] -> [[a]]
matTranspose = transpose

-- Get the number of columns in a matrix
numCols :: [[a]] -> Int
numCols a = length $ head a

-- Get the number of rows in a matrix
numRows :: [[a]] -> Int
numRows = length

-- Get column j from matrix
getCol :: [[a]] -> Int -> [a]
getCol mat j = map (!! j) mat

-- Get row i from matrix
getRow :: [[a]] -> Int -> [a]
getRow mat i = mat !! i