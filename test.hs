import Test.QuickCheck

import Octree

octantPositions :: [(Int,Int,Int)]
octantPositions = [(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]

heights :: Gen Int
heights = choose (1, 8)

leaves :: Gen Char
leaves = choose ('a', 'b')

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

main :: IO ()
main = do
    quickCheck prop_insert_notempty
    quickCheck prop_insert_distinct
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_collapse
    quickCheck prop_expandcollapse
