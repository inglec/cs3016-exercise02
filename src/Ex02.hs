{- butrfeld Andrew Butterfield -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data T k d
  = A (T k d) (T k d) k d -- Branch = (Left subtree) (Right subtree) key data
  | B k d                 -- Leaf
  | C                     -- Empty
  deriving (Eq, Show)

type IntFun = T Int Int -- binary tree with integer keys and data


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> T k d -> T k d

ins k d C = B k d -- If empty node, create new leaf.

ins k1 d1 (B k2 d2) -- If leaf.
    | k1 == k2 = B k1 d1 -- Replace data.
    | k1 < k2  = A (B k1 d1) C k2 d2 -- Create branch w/ new leaf on left.
    | k1 > k2  = A C (B k1 d1) k2 d2 -- Create branch w/ new leaf on right.

ins k1 d1 (A left right k2 d2)  -- If branch.
    | k1 == k2 = A left right k1 d1 -- Replace data.
    | k1 < k2  = A (ins k1 d1 left) right k2 d2 -- Recursively call for left subtree.
    | k1 > k2  = A left (ins k1 d1 right) k2 d2 -- Recursively call for right subtree.

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => T k d -> k -> m d

lkp C _ = fail "Empty leaf" -- Fail for empty leaf.

lkp (B k d) lkpKey -- If leaf.
    | lkpKey == k = return d -- Return data.
    | otherwise   = fail "Key does not match with leaf" -- Fail lookup.

lkp (A left right k d) lkpKey -- If branch.
    | lkpKey == k = return d         -- Return data.
    | lkpKey < k  = lkp left lkpKey  -- Search left subtree.
    | lkpKey > k  = lkp right lkpKey -- Search right subtree.

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone sum sumofsquares n -- sum = sum of items, sumofsquares = sum of items^2, n = num of items
    = (average, sqrt variance)
    where
        nd = fromInteger $ toInteger n
        average = sum / nd
        variance = (sumofsquares / nd) - (average * average)

{-
  Implement the following function to take a list of numbers  (Double)
  and return a triple containing
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)
   the length of the list (Int)
   You will need to figure out what extra arguments the function requires.
   Look at the tests for hints
-}
getsumsandlength :: Double -> Double -> Int -> [Double] -> (Double, Double, Int)
getsumsandlength aOffset bOffset cOffset list
    = (sumOfNumbers, sumOfSquares, lengthOfList)
    where
        sumOfNumbers = aOffset + foldl (+) 0 list
        sumOfSquares = bOffset + foldl (+) 0 (map (^2) list) -- (map (^2) list) squares each element of list
        lengthOfList = cOffset + length list
