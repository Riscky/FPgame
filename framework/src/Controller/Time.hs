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
timeHandler time w  = w >>= tship >>= tcollision >>= tspawnEnemy >>= tenemies >>= tshoot >>= tbullets >>= updatetime
      where (>>=)           :: World -> (Float -> World -> World) -> World
            (>>=)      w' f = f timeslice w'
            updatetime t w' = w'{timeLastFrame = t}
            timeslice       = time - timeLastFrame w

tship, tenemies, tbullets, tshoot, tspawnEnemy, tcollision:: Float -> World -> World

tship     time w@World{..} = let (so,sl) = (case rotateAction of
                                              RotateLeft  -> shiporientation - 5
                                              RotateRight -> shiporientation + 5
                                              NoRotation  -> shiporientation,
                                            update speed shiporientation shiplocation)
                             in w{shiplocation = sl, shiporientation = so}
                             where speed = if movementAction == NoMovement
                                            then 2.5
                                            else 4

tenemies  time w@World{..} = let enemies' = map (\(Enemy e) -> Enemy (update 1.5 (vecToDegree(dir e)) e)) enemies
                              in w{enemies = enemies'}
                           where dir e = shiplocation <-> e

tspawnEnemy time w@World{..} = if spawnNextEnemy == 0
                                then
                                  let ((g',x), y) = (random . fst $ random rndGen, snd $ random rndGen)
                                    in w{enemies = Enemy (x,y) : enemies, rndGen = g', spawnNextEnemy = 15}
                                else w{spawnNextEnemy = spawnNextEnemy - 1}
                              where random :: StdGen-> (StdGen, Float)
                                    random g = let (x, g') = randomR (15, 150) g
                                                in (g',x)


tbullets  time w@World{..} = let bullets' = map (\(Bullet dir pos) -> Bullet dir (update 5 dir pos)) bullets
                              in w{bullets = bullets'}

tshoot    time w@World{..} = let bullets' = if shootAction == Shoot
                                              then Bullet shiporientation shiplocation : bullets
                                              else bullets
                             in w{bullets = bullets'}

tcollision time w@World{..} = if dead
                                then initial 5 -- not a random value, should be changed later
                                else w
                            where dead = any (\(Enemy y) -> boxCollison y shiplocation 20) enemies


update                :: Float -> Float -> Point -> Point
update speed dir pos  = let vec = (speed * (- cos dir'), speed * sin dir')
                        in vec <+> pos
                        where dir' = degToRad dir + (pi / 2)

--vector math
infixl 6 <+>, <->
(<+>) a = (+) (fst a) *** (+) (snd a)
(<->) a = (-) (fst a) *** (-) (snd a)

boxCollison             :: Point -> Point -> Float -> Bool
boxCollison posa posb x = dot (posa - posb) < x

normalize       :: Vector -> Vector
normalize (a,b) = (a/c,b/c)
                where c = sqrt (a*a+b*b)

dot       :: Vector -> Float
dot (a,b) = a*a + b*b

vecToDegree       :: Vector -> Float
vecToDegree (a,b) = if b > 0
                    then rtd
                    else 180 + rtd
                    where rtd = radToDeg(atan(a/b))
