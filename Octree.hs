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

data OctNode a  = Node  { oct0, oct1, oct2, oct3, oct4, oct5, oct6, oct7 :: !(Octree a) }
                | Leaf  !a
                | Empty
                deriving (Eq, Show)

data Octree a   = Octree !Int (OctNode a)
                deriving (Eq, Show)

type Position   = (Int, Int, Int)

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
    where
        label                                       = bit' x 4 .|. bit' y 2 .|. bit' z 1
        bit' a b                                    = if testBit a h' then b else 0 :: Int
        h'                                          = h - 1

expand :: Octree a -> Octree a
expand (Octree h node)                              = Octree h $ case node of
                                                                     Empty  -> e Empty
                                                                     Leaf v -> e (Leaf v)
                                                                     _      -> node
    where
        e n                                         = Node o o o o o o o o
            where
                o                                   = Octree (h - 1) n

collapse :: Eq a => Octree a -> Octree a
collapse (Octree h (Node o0 o1 o2 o3 o4 o5 o6 o7))
    | all (==o0) [o1,o2,o3,o4,o5,o6,o7]             = let (Octree _ node) = o0 in Octree h node
collapse octree                                     = octree

lookup :: Eq a => Position -> Octree a -> Octree a
lookup pos octree@(Octree h (Node o0 o1 o2 o3 o4 o5 o6 o7))
                                                    = case step h pos of Nothing -> octree
                                                                         Just O0 -> l o0
                                                                         Just O1 -> l o1
                                                                         Just O2 -> l o2
                                                                         Just O3 -> l o3
                                                                         Just O4 -> l o4
                                                                         Just O5 -> l o5
                                                                         Just O6 -> l o6
                                                                         Just O7 -> l o7
    where l                                         = lookup pos

modify :: Eq a => (Octree a -> Octree a) -> Position -> Octree a -> Octree a
modify f pos                                        = collapse . modify' . expand
    where
        modify' octree@(Octree h node@Node{})       = case step h pos of Nothing -> f octree
                                                                         Just O0 -> m oct0 (\o x->o {oct0=x})
                                                                         Just O1 -> m oct1 (\o x->o {oct1=x})
                                                                         Just O2 -> m oct2 (\o x->o {oct2=x})
                                                                         Just O3 -> m oct3 (\o x->o {oct3=x})
                                                                         Just O4 -> m oct4 (\o x->o {oct4=x})
                                                                         Just O5 -> m oct5 (\o x->o {oct5=x})
                                                                         Just O6 -> m oct6 (\o x->o {oct6=x})
                                                                         Just O7 -> m oct7 (\o x->o {oct7=x})
            where m g s                             = Octree h . s node . modify f pos . g $ node

empty :: Int -> Octree a
empty h = Octree h Empty

set :: Eq a => Position -> Octree a -> Octree a -> Octree a
set pos octree = modify (const octree) pos

insert :: Eq a => Position -> a -> Octree a -> Octree a
insert pos val = set pos (Octree 0 (Leaf val))

delete :: Eq a => Position -> Octree a -> Octree a
delete pos = set pos (Octree 0 Empty)

-- toList :: Octree a -> [a]
-- toList Empty = []
-- toList (Leaf a) = [a]
-- toList (Node (Octants a b c d e f g h)) = concatMap toList [a,b,c,d,e,f,g,h]
