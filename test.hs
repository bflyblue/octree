import Test.QuickCheck

import Octree

import Data.Bits
import Data.List (sort)
import qualified Data.Map as M
import System.Random

octantPositions :: [Position]
octantPositions = [(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]

heights :: Gen Int
heights = choose (1, 8)

leaves :: Gen Char
leaves = choose ('a', 'b')

position :: Int -> Gen Position
position n = do
    x <- choose (0, bit n - 1)
    y <- choose (0, bit n - 1)
    z <- choose (0, bit n - 1)
    return (x,y,z)

rect :: Int -> Gen (Position, Position)
rect n = do
    p1 <- position n
    p2 <- position n
    return (p1,p2)

values' :: Int -> Int -> Gen (Position, Int)
values' n r = do
    p <- position n
    v <- choose(0, r)
    return (p, v)

-- something gets inserted
prop_insert_notempty :: Char -> Int -> Int -> Int -> Property
prop_insert_notempty a x y z = forAll heights $ \h ->
    insert (x,y,z) a (empty h) /= empty h

-- distinct positions yield district results
prop_insert_distinct :: Bool
prop_insert_distinct =
    let
        o = map (\p -> insert p 'a' (empty 1)) octantPositions
    in
        all (== 1) $ map (\a -> length . filter (==a) $ o) o

-- collapse when all leaves are equal
prop_collapse :: Property
prop_collapse = forAll (vectorOf 8 leaves) $ \ls ->
    let
        o = foldr (\(l,p) ot -> insert p l ot) (empty 1) (zip ls octantPositions)
        l1 = head ls
    in
        (o == Leaf 1 l1) == (all (== l1) ls)

-- insert below a Leaf will collapse back to Leaf if it matched
prop_expandcollapse :: Property
prop_expandcollapse = forAll (vectorOf 2 leaves) $ \ls ->
    let (l1:l2:[]) = ls
        o = map (\p -> insert p l1 (Leaf 1 l2)) octantPositions
    in
        (all (== Leaf 1 l1) o) == (l1 == l2)

(@==) :: Ord a => [a] -> [a] -> Bool
a @== b = sort a == sort b

prop_identity :: Property
prop_identity = forAll (listOf (values' 4 4)) $ \ds ->
    (toList . fromList 4 $ ds) @== (M.toList . M.fromList $ ds)

area :: Position -> Position -> Int
area (x,y,z) (x',y',z') =
    (abs (x - x') + 1) * (abs (y - y') + 1) * (abs (z - z') + 1)

prop_sum :: Property
prop_sum = forAll (rect 8) $ \(p1, p2) ->
    let ot = fill p1 p2 1 (Leaf 8 0)
    in
        -- sum [lookupDefault 0 (x,y,z) ot | let r = [0..255], x <- r, y <- r, z <- r] == area p1 p2
        sum (values ot) == area p1 p2

main :: IO ()
main = do
    let gen = mkStdGen 1
    let lots = stdArgs { maxSuccess = 10000 }
        replay = stdArgs { replay = Just (gen, 123), maxSuccess = 5 }
        few  = stdArgs { maxSuccess = 10 }
    -- quickCheck prop_insert_notempty
    -- quickCheck prop_insert_distinct
    -- quickCheckWith lots prop_collapse
    -- quickCheck prop_expandcollapse
    -- quickCheckWith lots prop_identity
    quickCheckWith replay prop_sum
