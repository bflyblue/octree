{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Octree where

import Prelude hiding (lookup)
import Data.Bits

data Octant     = NWU | NEU | SWU | SEU | NWD | NED | SWD | SED
                deriving (Eq, Ord, Enum, Show)

data Octree a   = Node  { nwu, neu
                        , swu, seu
                        , nwd, ned
                        , swd, sed  :: !(Octree a)
                        }
                | Leaf  !a
                | Empty
                deriving (Eq, Show)

type Position = (Int, Int, Int)

pathToPos :: Int -> Position -> [Octant]
pathToPos 0 _         = []
pathToPos h (x, y, z) =
    let h' = h - 1
        sigbit = flip testBit h'
        (i, j, k)  = (sigbit x, sigbit y, sigbit z)
        path' = pathToPos h' (x, y, z)
    in case (i, j, k) of
        (False, True , True )   -> NWU : path'
        (True , True , True )   -> NEU : path'
        (False, False, True )   -> SWU : path'
        (True , False, True )   -> SEU : path'
        (False, True , False)   -> NWD : path'
        (True , True , False)   -> NED : path'
        (False, False, False)   -> SWD : path'
        (True , False, False)   -> SED : path'

expand :: Octree a -> Octree a
expand Empty    = Node Empty Empty Empty Empty Empty Empty Empty Empty
expand (Leaf v) = Node (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v)
expand n        = n

collapse :: Eq a => Octree a -> Octree a
collapse (Node a b c d e f g h) | all (==a) [b,c,d,e,f,g,h] = a
collapse n                                                  = n

octant :: Octant -> Octree a -> Octree a
octant NWU = nwu
octant NEU = neu
octant SWU = swu
octant SEU = seu
octant NWD = nwd
octant NED = ned
octant SWD = swd
octant SED = sed

lookup :: Eq a => [Octant] -> Octree a -> Octree a
lookup (o:os) n@Node{} = lookup os (octant o n)
lookup _      ot       = ot

modify :: Eq a => (Octree a -> Octree a) -> [Octant] -> Octree a -> Octree a
modify f []     = f
modify f (o:os) = collapse . modify' . expand
    where   modify' n@Node{} =
                let m = modify f os (octant o n) in
                case o of
                    NWU -> n { nwu = m }
                    NEU -> n { neu = m }
                    SWU -> n { swu = m }
                    SEU -> n { seu = m }
                    NWD -> n { nwd = m }
                    NED -> n { ned = m }
                    SWD -> n { swd = m }
                    SED -> n { sed = m }
            modify' _ = error "expand didn't return node"

set :: Eq a => Octree a -> [Octant] -> Octree a -> Octree a
set v = modify (const v)

insert :: Eq a => a -> [Octant] -> Octree a -> Octree a
insert = set . Leaf

delete :: Eq a => [Octant] -> Octree a -> Octree a
delete = set Empty
