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
timeHandler time w  = w >>= tship >>= tenemies >>= tshoot >>= tbullets >>= updatetime
      where (>>=)           :: World -> (Float -> World -> World) -> World
            (>>=)      w' f = f timeslice w'
            updatetime t w' = w'{timeLastFrame = t}
            timeslice       = time - timeLastFrame w

tship, tenemies, tbullets, tshoot :: Float -> World -> World

tship     time w@World{..} = let (so,sl) = (case rotateAction of
                                              RotateLeft  -> shiporientation - 2
                                              RotateRight -> shiporientation + 2
                                              NoRotation  -> shiporientation,
                                            update 5 shiporientation shiplocation)
                             in w{shiplocation = sl, shiporientation = so}

tenemies  time w@World{..} = let enemies' = map (\(Enemy e) -> Enemy (update 6 (vecToDegree(dir e)) e)) enemies
                              in w{enemies = enemies'}
                           where dir e = shiplocation <-> e

tbullets  time w@World{..} = let bullets' = map (\(Bullet dir pos) -> Bullet dir (update 10 dir pos)) bullets
                              in w{bullets = bullets'}

tshoot    time w@World{..} = let bullets' = if shootAction == Shoot
                                              then Bullet shiporientation shiplocation : bullets
                                              else bullets
                             in w{bullets = bullets'}


update                :: Float -> Float -> Point -> Point
update speed dir pos  = let vec = (- cos dir', sin dir')
                        in vec <+> pos
                        where dir' = degToRad dir + (pi / 2)

--vector math
infixl 6 <+>, <->
(<+>) a = (+) (fst a) *** (+) (snd a)
(<->) a = (-) (fst a) *** (-) (snd a)

normalize       :: Vector -> Vector
normalize (a,b) = (a/c,b/c)
                where c = sqrt (a*a+b*b)

vecToDegree       :: Vector -> Float
vecToDegree (a,b) = radToDeg(atan(a/b))
