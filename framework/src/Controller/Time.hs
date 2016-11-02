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

import Config

-- | Time handling

timeHandler        :: Float -> World -> World
timeHandler time w = w  >>= engineParticles   >>= tparticles  >>= tship
                        >>= collisionHandler  >>= tspawnEnemy >>= tspawnBonus
                        >>= tenemies          >>= tshoot      >>= tbullets
                        >>= tspawnStars       >>= tmoveStars  >>= updatetime
                        >>= respawnChecker
                   where (>>=)      :: World -> (Float -> World -> World) -> World
                         (>>=) w' f = f timeslice w'
                         updatetime t w' = w'{timeLastFrame = t}
                         timeslice = time - timeLastFrame w


collisionHandler   :: Float -> World -> World
collisionHandler _ = shipcoll . encoll . bonuscoll

shipcoll, shipcollEnemy, shipcollBonus,encoll, bonuscoll :: World -> World

shipcoll = shipcollEnemy . shipcollBonus
shipcollEnemy  w@World{..} = if dead
                          then w{dead = True} -- Add the particles upon death
                          else w
                      where dead = any (\(Enemy y) -> boxCollision y shiplocation 40) enemies

shipcollBonus w@World{..} = if score > 0
                            then w{multiplier = multiplier + score, bonusses = filter (not . cond) bonusses}
                            else w
                          where score = length $ filter cond bonusses
                                cond (Bonus y) = boxCollision shiplocation y 140

encoll w@World{..} = let enemies' = map Enemy multcoll
                      in  w{enemies = enemies', score = score + multiplier * (length enemies - length enemies'), particles = snd ps ++ particles, rndGen = fst ps}
                    where multcoll = filter (not . p) es'
                          p a = not (null bs') &&
                                any (\b -> boxCollision a b 100) bs'
                          es' = map unEnemy enemies
                          bs' = map (\(Bullet _ x) -> x) bullets
                          ps  = foldr (\pos (g, ps') -> ins (dyingParticles 3 pos g) ps') (rndGen, []) $ filter p es'
                          ins (a,ps) ps' = (a,ps ++ ps')

bonuscoll w@World{..} = let bonusses' = map Bonus $ multcoll bonusses bullets unBonus (\(Bullet _ x) -> x) 90
                        in  w{bonusses = bonusses'}

multcoll :: [a] -> [b] -> (a -> Point) -> (b -> Point) -> Float -> [Point]
multcoll as bs fa fb dist = filter p as'
                          where p a = not (not (null bs') &&
                                      any (\b -> boxCollision a b dist) bs')
                                bs' = map fb bs
                                as' = map fa as

tship, tenemies, tbullets, tshoot, tspawnEnemy, tspawnBonus, tparticles, engineParticles:: Float -> World -> World

respawnChecker _ w@World{..} = if dead && continue then initial $ truncate $ snd $ randomFloat 0 100000 rndGen else w

tship  time w@World{..} | not dead = let (so,sl) = (shiporientation + ospeed,
                                                          update speed shiporientation shiplocation)
                                      in w{shiplocation = sl, shiporientation = so, angularVelocity = ospeed}
                        | otherwise     = w
                                    where speed = if movementAction == NoMovement
                                                   then 250.0 * time
                                                   else 400.0 * time
                                          ospeed = case rotateAction of
                                                             RotateLeft  -> (-5) * 10.0 * time + (1.0 - 10.0 * time) *angularVelocity + drag
                                                             RotateRight -> 5 * 10.0 * time + (1.0 -10.0 * time ) *angularVelocity + drag
                                                             NoRotation  -> angularVelocity + drag
                                          drag = if angularVelocity < 0 then 14.0 * time else 14.0 * (-time)

tenemies  time w@World{..} = let enemies' = map (\(Enemy e) -> Enemy (update (150 * time) (vecToDegree(dir e)) e)) enemies
                              in w{enemies = enemies'}
                           where dir e = shiplocation <-> e

tspawnEnemy time w@World{..} = if spawnNextEnemy - time < 0
                                then
                                  let (pos, g') = randomPos rndGen
                                    in w{enemies = Enemy pos : enemies, rndGen = g', spawnNextEnemy = 0.1}
                                else w{spawnNextEnemy = spawnNextEnemy - time}

randomPos   :: StdGen -> (Point,StdGen)
randomPos g = let ((g',x),y) = (random' . fst $ random' g, snd $ random' g)
              in ((x,y),g')
              where random' = randomFloat (-500) 500

randomFloat :: Float -> Float -> StdGen -> (StdGen, Float)
randomFloat lo hi g = let (x, g') = randomR (lo, hi) g
                    in (g',x)

tspawnBonus time w@World{..} = if spawnNextBonus == 0
                                then let (pos,g') = randomPos rndGen
                                in w{bonusses = Bonus pos : bonusses, rndGen = g', spawnNextBonus = 150}
                              else w{spawnNextBonus = spawnNextBonus - 1}

engineParticles time w@World{..}  = if not dead then w{particles = newparticle : particles, rndGen = g} else w
                                        where newparticle = Particle dir (update 10 dir shiplocation) (greyN 6) shps 0.35
                                              (dir,g)     = (shiporientation + snd random' + 180.0, fst random')
                                              random'     = randomFloat (-10) 10 rndGen
                                              shps = if movementAction == NoMovement
                                                       then 2.5
                                                       else 4.0

dyingParticles :: Float -> Point -> StdGen -> (StdGen, [Particle])
dyingParticles time pos rndGen = foldr newParticle (rndGen, [])  [0 .. 100]
                                where
                                  newParticle _ (g, ps) = (snd $ np g, fst (np g) : ps)
                                  np g = (Particle (snd $ dir g) pos red 2 0.3, fst $ dir g)
                                  dir = randomFloat 0 360

tparticles time w@World{..} = w{particles = particles'}
                            where particles' = map updatePos $ filter p $ map (\p@Particle{..} -> p{dietime = dietime - time}) particles
                                  p Particle{..} = dietime > 0
                                  updatePos p@Particle{..} = p{position = update speed direction position}

tspawnStars :: Float -> World -> World
tspawnStars time w@World{..} = if spawnNextStar - time < 0
                                then w{stars = stars', rndGen = g3', spawnNextStar = gen3}
                                else w{spawnNextStar = spawnNextStar - time}
                              where (g1', gen1) = randomFloat 0.2 3.0 rndGen
                                    str = Star (negate defaultHorizontalResolution/2 , gen2) gen1
                                    (g2', gen2) = randomFloat (negate defaultVerticalResolution /2) defaultVerticalResolution g1'
                                    (g3', gen3) = randomFloat 0 0.1 g2'
                                    stars' = str : stars

tmoveStars :: Float -> World -> World
tmoveStars time w@World{..} = let stars' = map (\(Star p s) -> Star (update s 116.57 p) s) stars
                               in w{stars = stars'}


tbullets  time w@World{..} = let bullets' = map (\(Bullet dir pos) -> Bullet dir (update 5 dir pos)) bullets
                              in w{bullets = bullets'}


tshoot    time w@World{..} = let bullets' =  if cond
                                              then Bullet shiporientation shiplocation : bullets
                                              else bullets
                             in w{bullets = bullets', spawnNextBullet =  if cond then 0.05 else spawnNextBullet - time}
                                    where cond = shootAction == Shoot && spawnNextBullet - time < 0 && not dead -- Time based re-firing contr

update                :: Float -> Float -> Point -> Point
update speed dir pos  = let vec = (speed * (- cos dir'), speed * sin dir')
                        in vec <+> pos
                        where dir' = degToRad dir + (pi / 2)


--vector math
infixl 6 <+>, <->
(<+>) a = (+) (fst a) *** (+) (snd a)
(<->) a = (-) (fst a) *** (-) (snd a)

boxCollision             :: Point -> Point -> Float -> Bool
boxCollision posa posb x = dot (posa - posb) < x

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
