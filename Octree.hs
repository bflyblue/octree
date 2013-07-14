{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Octree where

import Prelude hiding (lookup)
import Data.Bits

--     3---7
--    /|  /|
--   2---6 |
--   | 1-|-5
--   |/  |/   Y Z
--   0---4    |/_X

data Octant     = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7
                deriving (Eq, Ord, Enum, Show)

data Octree a   = Node  { oct0, oct1, oct2, oct3, oct4, oct5, oct6, oct7 :: !(Octree a) }
                | Leaf  !a
                | Empty
                deriving (Eq, Show)

type Position   = (Int, Int, Int)

pathToPos :: Int -> Position -> [Octant]
pathToPos 0 _         = []
pathToPos h (x, y, z) =
    let subnode = case label of 0 -> O0
                                1 -> O1
                                2 -> O2
                                3 -> O3
                                4 -> O4
                                5 -> O5
                                6 -> O6
                                7 -> O7
    in subnode : pathToPos h' (x, y, z)
    where
        label       = bit' x 4 .|. bit' y 2 .|. bit' z 1
        bit' a b    = if testBit a h' then b else 0 :: Int
        h'          = h - 1

expand :: Octree a -> Octree a
expand Empty                            = Node Empty Empty Empty Empty Empty Empty Empty Empty
expand (Leaf v)                         = Node (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v)
expand node                             = node

collapse :: Eq a => Octree a -> Octree a
collapse (Node a b c d e f g h)
        | all (==a) [b,c,d,e,f,g,h]     = a
collapse node                           = node

lookup :: Eq a => [Octant] -> Octree a -> Octree a
lookup (o:os) node@Node{}               = let l o' = lookup os (o' node) in
                                          case o of O0 -> l oct0
                                                    O1 -> l oct1
                                                    O2 -> l oct2
                                                    O3 -> l oct3
                                                    O4 -> l oct4
                                                    O5 -> l oct5
                                                    O6 -> l oct6
                                                    O7 -> l oct7
lookup _      ot                        = ot

modify :: Eq a => (Octree a -> Octree a) -> [Octant] -> Octree a -> Octree a
modify f []                             = f
modify f (o:os)                         = collapse . modify' . expand
    where
        modify' node                    = let m o' = modify f os (o' node) in
                                          case o of O0 -> node { oct0 = m oct0 }
                                                    O1 -> node { oct1 = m oct1 }
                                                    O2 -> node { oct2 = m oct2 }
                                                    O3 -> node { oct3 = m oct3 }
                                                    O4 -> node { oct4 = m oct4 }
                                                    O5 -> node { oct5 = m oct5 }
                                                    O6 -> node { oct6 = m oct6 }
                                                    O7 -> node { oct7 = m oct7 }

empty :: Octree a
empty = Empty

set :: Eq a => Octree a -> [Octant] -> Octree a -> Octree a
set v = modify (const v)

insert :: Eq a => a -> [Octant] -> Octree a -> Octree a
insert = set . Leaf

delete :: Eq a => [Octant] -> Octree a -> Octree a
delete = set Empty

-- toList :: Octree a -> [a]
-- toList Empty = []
-- toList (Leaf a) = [a]
-- toList (Node (Octants a b c d e f g h)) = concatMap toList [a,b,c,d,e,f,g,h]
