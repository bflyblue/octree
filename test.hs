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
    insert a (pathToPos h (x,y,z)) empty /= empty

-- distinct positions yield district results
prop_insert_distinct :: Bool
prop_insert_distinct =
    let
        o = map (\p -> insert 'a' (pathToPos 1 p) empty) octantPositions
    in
        all (== 1) $ map (\a -> length . filter (==a) $ o) o

-- collapse when all leaves are equal
prop_collapse :: Property
prop_collapse = forAll (vectorOf 8 leaves) $ \ls ->
    let
        o = foldr (\(l,p) ot -> insert l (pathToPos 1 p) ot) empty (zip ls octantPositions)
        l1 = head ls
    in
        (o == Leaf l1) == (all (== l1) ls)

-- insert below a Leaf will collapse back to Leaf if it matched
prop_expandcollapse :: Property
prop_expandcollapse = forAll (vectorOf 2 leaves) $ \ls ->
    let (l1:l2:[]) = ls
        o = map (\p -> insert l1 (pathToPos 1 p) (Leaf l2)) octantPositions
    in
        (all (== Leaf l1) o) == (l1 == l2)

main :: IO ()
main = do
    quickCheck prop_insert_notempty
    quickCheck prop_insert_distinct
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_collapse
    quickCheck prop_expandcollapse
