{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Octree where

import Prelude hiding (lookup)
import Data.Bits
import Data.List (foldl')
import Data.Maybe (fromMaybe)

--     3---7
--    /|  /|
--   2---6 |
--   | 1-|-5
--   |/  |/   Y Z
--   0---4    |/_X

data Octant     = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7
                deriving (Eq, Ord, Enum, Show)

data Octree a   = Node  { height                    :: !Int
                        , oct0, oct1, oct2, oct3
                        , oct4, oct5, oct6, oct7    :: !(Octree a)
                        }
                | Leaf  { height                    :: !Int
                        , leaf                      :: !a
                        }
                | Empty { height                    :: !Int
                        }
                deriving (Eq, Show)

type Position   = (Int, Int, Int)
type Dimensions = (Int, Int, Int, Int)

step :: Int -> Position -> Maybe Octant
step 0 _                                            = Nothing
step h (x, y, z)                                    = Just $ case label of 0 -> O0
                                                                           1 -> O1
                                                                           2 -> O2
                                                                           3 -> O3
                                                                           4 -> O4
                                                                           5 -> O5
                                                                           6 -> O6
                                                                           7 -> O7
                                                                           _ -> error "label out of range"
    where
        label                                       = bit' x 4 .|. bit' y 2 .|. bit' z 1
        bit' a b                                    = if testBit a h' then b else 0 :: Int
        h'                                          = h - 1

subdivide :: Dimensions -> Octant -> Dimensions
subdivide (_, _, _, 0) _      = error "cannot subdivide level 0 node"
subdivide (x, y, z, h) octant =
    let h' = h - 1
        xm = x .|. bit h'
        ym = y .|. bit h'
        zm = z .|. bit h'
    in
        case octant of O0 -> (x , y , z , h')
                       O1 -> (x , y , zm, h')
                       O2 -> (x , ym, z , h')
                       O3 -> (x , ym, zm, h')
                       O4 -> (xm, y , z , h')
                       O5 -> (xm, y , zm, h')
                       O6 -> (xm, ym, z , h')
                       O7 -> (xm, ym, zm, h')

expand :: Octree a -> Octree a
expand (Empty h)                                    = Node h e e e e e e e e where e = Empty (h - 1)
expand (Leaf h v)                                   = Node h l l l l l l l l where l = Leaf (h - 1) v
expand octree                                       = octree

collapse :: Eq a => Octree a -> Octree a
collapse (Node h o0 o1 o2 o3 o4 o5 o6 o7)
    | all (==o0) [o1,o2,o3,o4,o5,o6,o7]             = case o0 of Leaf _ v                       -> Leaf h v
                                                                 Empty _                        -> Empty h
                                                                 Node _ n1 n2 n3 n4 n5 n6 n7 n8 -> Node h n1 n2 n3 n4 n5 n6 n7 n8
collapse octree                                     = octree

lookup :: Eq a => Position -> Octree a -> Maybe a
lookup pos (Node h o0 o1 o2 o3 o4 o5 o6 o7)         = case step h pos of Nothing -> Nothing
                                                                         Just O0 -> l o0
                                                                         Just O1 -> l o1
                                                                         Just O2 -> l o2
                                                                         Just O3 -> l o3
                                                                         Just O4 -> l o4
                                                                         Just O5 -> l o5
                                                                         Just O6 -> l o6
                                                                         Just O7 -> l o7
    where l                                         = lookup pos
lookup _   Empty{}                                  = Nothing
lookup _   (Leaf _ val)                             = Just val

lookupDefault :: Eq a => a -> Position -> Octree a -> a
lookupDefault defval pos octree                     = fromMaybe defval (lookup pos octree)

modify :: Eq a => (Octree a -> Octree a) -> Position -> Octree a -> Octree a
modify f pos                                        = collapse . modify' . expand
    where
        modify' octree@(Node h o0 o1 o2 o3 o4 o5 o6 o7)
                                                    = case step h pos of Nothing -> f octree
                                                                         Just O0 -> m o0 (\o x->o {oct0=x})
                                                                         Just O1 -> m o1 (\o x->o {oct1=x})
                                                                         Just O2 -> m o2 (\o x->o {oct2=x})
                                                                         Just O3 -> m o3 (\o x->o {oct3=x})
                                                                         Just O4 -> m o4 (\o x->o {oct4=x})
                                                                         Just O5 -> m o5 (\o x->o {oct5=x})
                                                                         Just O6 -> m o6 (\o x->o {oct6=x})
                                                                         Just O7 -> m o7 (\o x->o {oct7=x})
            where m n s                             = s octree . modify f pos $ n
        modify' _                                   = error "expand did not return a node"

empty :: Int -> Octree a
empty = Empty

set :: Eq a => Position -> Octree a -> Octree a -> Octree a
set pos octree = modify (const octree) pos

insert :: Eq a => Position -> a -> Octree a -> Octree a
insert pos val = set pos (Leaf 0 val)

delete :: Eq a => Position -> Octree a -> Octree a
delete pos = set pos (Empty 0)

initDim :: Octree a -> Dimensions
initDim octree = (0, 0, 0, height octree)

walk :: Eq a => (Octree a -> Dimensions -> [b]) -> Octree a -> [b]
walk f octree = walk' (initDim octree) octree
    where
        walk' dim octree' =
            case octree' of
                Empty{} -> f octree' dim
                Leaf{}  -> f octree' dim
                Node 0 _  _  _  _  _  _  _  _  -> error "node at level 0?"
                Node _ n0 n1 n2 n3 n4 n5 n6 n7 ->
                    f octree dim ++ w O0 n0 ++ w O1 n1 ++ w O2 n2 ++ w O3 n3 ++ w O4 n4 ++ w O5 n5 ++ w O6 n6 ++ w O7 n7
            where
                w o     = walk' (subdivide dim o)

fill :: Eq a => Position -> Position -> a -> Octree a -> Octree a
fill (x,y,z) (x',y',z') val octree =
    let order a a' = (min a a', max a a')
        (i,i') = order x x'
        (j,j') = order y y'
        (k,k') = order z z'
    in
        fill' (i,j,k) (i',j',k') (initDim octree) val octree

fill' :: Eq a => Position -> Position -> Dimensions -> a -> Octree a -> Octree a
fill' (x,y,z) (x',y',z') dim@(i,j,k,h) val octree
    | x' < i || y' < j || z' < k ||
      x > i' || y > j' || z > k'        = octree
    | x <= i && y <= j && z <= k &&
      x' >= i' && y' >= j' && z' >= k'  = Leaf h val
    | otherwise                         = collapse node'
    where
        f o                             = fill' (x,y,z) (x', y', z') (subdivide dim o) val
        Node _ o0 o1 o2 o3 o4 o5 o6 o7  = expand octree
        node'                           = Node h (f O0 o0) (f O1 o1) (f O2 o2) (f O3 o3) (f O4 o4) (f O5 o5) (f O6 o6) (f O7 o7)
        (i',j',k')                      = (i + d, j + d, k + d)
        d                               = bit h - 1

toList :: Eq a => Octree a -> [(Position, a)]
toList = walk l'
    where l' (Leaf _ v) (x,y,z,h)   = [((x+i, y+j, z+k), v) | let r = [0..bit h-1], i <- r, j <- r, k <- r]
          l' _   _                  = []

toDim :: Eq a => Octree a -> [(Dimensions, a)]
toDim = walk l'
    where l' (Leaf _ v) (x,y,z,h)   = [((x,y,z,h), v)]
          l' _   _                  = []

fromList :: Eq a => Int -> [(Position, a)] -> Octree a
fromList h = foldl' (\o (p,v) -> insert p v o) (empty h)
