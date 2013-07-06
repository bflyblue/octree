{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Octree where

import Prelude hiding (lookup)
import Data.Bits

data Octant     = NWU | NEU | SWU | SEU | NWD | NED | SWD | SED
                deriving (Eq, Ord, Enum, Show)

data Octants a  = Octants   { nwu, neu
                            , swu, seu
                            , nwd, ned
                            , swd, sed  :: !(Octree a)
                            }
                deriving (Eq, Show)

data Octree a   = Node  (Octants a)
                | Leaf  !a
                | Empty
                deriving (Eq, Show)

type Position = (Int, Int, Int)

pathToPos :: Int -> Position -> [Octant]
pathToPos 0 _         = []
pathToPos h (x, y, z) =
    case sigbits of
        (False, True , True )   -> NWU : path'
        (True , True , True )   -> NEU : path'
        (False, False, True )   -> SWU : path'
        (True , False, True )   -> SEU : path'
        (False, True , False)   -> NWD : path'
        (True , True , False)   -> NED : path'
        (False, False, False)   -> SWD : path'
        (True , False, False)   -> SED : path'
    where
        sigbits = (sigbit x, sigbit y, sigbit z)
        sigbit  = flip testBit h'
        path'   = pathToPos h' (x, y, z)
        h'      = h - 1

expand :: Octree a -> Octants a
expand Empty    = Octants Empty Empty Empty Empty Empty Empty Empty Empty
expand (Leaf v) = Octants (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v) (Leaf v)
expand (Node o) = o

collapse :: Eq a => Octants a -> Octree a
collapse (Octants a b c d e f g h) | all (==a) [b,c,d,e,f,g,h] = a
collapse o                                                     = Node o

lookup :: Eq a => [Octant] -> Octree a -> Octree a
lookup (o:os) (Node ocs) =
    let l o' = lookup os (o' ocs) in
    case o of
        NWU -> l nwu
        NEU -> l neu
        SWU -> l swu
        SEU -> l seu
        NWD -> l nwd
        NED -> l ned
        SWD -> l swd
        SED -> l sed
lookup _      ot       = ot

modify :: Eq a => (Octree a -> Octree a) -> [Octant] -> Octree a -> Octree a
modify f []     = f
modify f (o:os) = collapse . modify' . expand
    where
        modify' ocs =
            let m o' = modify f os (o' ocs) in
            case o of
                NWU -> ocs { nwu = m nwu }
                NEU -> ocs { neu = m neu }
                SWU -> ocs { swu = m swu }
                SEU -> ocs { seu = m seu }
                NWD -> ocs { nwd = m nwd }
                NED -> ocs { ned = m ned }
                SWD -> ocs { swd = m swd }
                SED -> ocs { sed = m sed }

set :: Eq a => Octree a -> [Octant] -> Octree a -> Octree a
set v = modify (const v)

insert :: Eq a => a -> [Octant] -> Octree a -> Octree a
insert = set . Leaf

delete :: Eq a => [Octant] -> Octree a -> Octree a
delete = set Empty
