{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time w = w >>= tship >>= tenemies >>= tbullets
      where (>>=) :: World -> (Float -> World -> World) -> World
            (>>=) w' f = f time w'

tship, tenemies, tbullets :: Float -> World -> World
tship     time w@World{..} = let so = case rotateAction of
                                        RotateLeft  -> shiporientation - 2
                                        RotateRight -> shiporientation + 2
                                        NoRotation  -> shiporientation
                             in w{shiplocation = sl, shiporientation = so}
                             where sl = undefined

tenemies  time w = undefined

tbullets  time w = undefined
