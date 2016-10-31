{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>),(***))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time w  = w >>= tship >>= tenemies >>= tbullets >>= updatet
      where (>>=)           :: World -> (Float -> World -> World) -> World
            (>>=)     w' f = f time w'
            updatet t w'   = w'{timeLastFrame = t}

tship, tenemies, tbullets :: Float -> World -> World

tship     time w@World{..} = let (so,sl) = (case rotateAction of
                                              RotateLeft  -> shiporientation - 2
                                              RotateRight -> shiporientation + 2
                                              NoRotation  -> shiporientation,
                                            update 5 shiporientation shiplocation)
                             in w{shiplocation = sl, shiporientation = so}

tenemies  time w@World{..} = w

tbullets  time w@World{..} = w

update                :: Float -> Float -> Point -> Point
update speed dir pos  = let vec = (cos dir', sin dir')
                        in ((+) (fst vec) *** (+) (snd vec)) pos
                        where dir' = degToRad dir
