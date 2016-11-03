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

-- Time handling

timeHandler        :: Float -> World -> World
timeHandler time w = w  >>= engineParticles   >>= tparticles      >>= tship
                        >>= collisionHandler  >>= tspawnEnemy     >>= tspawnBonus
                        >>= tenemies          >>= tshoot          >>= tbullets
                        >>= tspawnStars       >>= tmoveStars      >>= updatetime
                        >>= respawnChecker    >>= boundaryCleanup >>= tAliveUpdate
                   where (>>=)      :: World -> (Float -> World -> World) -> World
                         (>>=) w' f = f timeslice w'
                         updatetime t w' = w'{timeLastFrame = t}
                         timeslice = time - timeLastFrame w

-- Collision handling

collisionHandler   :: Float -> World -> World
collisionHandler _ = shipcollBonus . shipcollEnemy . encoll . bonuscoll

shipcollEnemy, shipcollBonus, encoll, bonuscoll :: World -> World

shipcollEnemy w@World{..} = if dead'  -- Spawn particles on dead, set dead = True to stop rendering of ship
                              then w{ dead = True,
                                      particles = snd deadparticles ++ particles,
                                      rndGen = fst deadparticles}
                              else w
                          where dead'         = not dead && any (\(Enemy y) -> boxCollision y shiplocation 40) enemies
                                deadparticles = dyingParticles shiplocation red rndGen

shipcollBonus w@World{..} = if score > 0
                              then w{ multiplier  = multiplier + score,
                                      bonusses    = filter (not . cond) bonusses}
                              else w
                          where score             = length $ filter cond bonusses
                                cond (Bonus y)    = boxCollision shiplocation y 140

encoll w@World{..}  = let enemies'        = map Enemy multcoll
                      in  w{enemies       = enemies',
                            score         = score + multiplier * (length enemies - length enemies'),
                            particles     = snd ps ++ particles,
                            rndGen        = fst ps}
                    where multcoll        = filter (not . cond) es'
                          cond a          = not (null bs') &&
                                            any (\b -> boxCollision a b 100) bs'
                          es'             = map unEnemy enemies
                          bs'             = map (\(Bullet _ x) -> x) bullets
                          ps              = foldr (\pos (g, ps') -> ins (dyingParticles pos blue g) ps') (rndGen, []) $ filter cond es'
                          ins (a,ps) ps'  = (a,ps ++ ps')

bonuscoll w@World{..} = let bonusses'     = map Bonus coll
                        in  w{bonusses    = bonusses',
                              particles   = snd ps ++ particles,
                              rndGen      = fst ps}
                      where
                          coll            = filter (not . cond) bonusses'
                          cond a          = not (null bullets') &&
                                            any (\b -> boxCollision a b 90) bullets'
                          bullets'        = map (\(Bullet _ x) -> x)  bullets
                          bonusses'       = map unBonus               bonusses
                          ps              = foldr (\pos (g, ps') -> ins (dyingParticles pos green g) ps') (rndGen, []) $ filter cond bonusses'
                          ins (a,ps) ps'  = (a,ps ++ ps')

-- Spawning

tspawnEnemy, tspawnBonus, tspawnStars, tshoot :: Float -> World -> World

tspawnEnemy time w@World{..} = if spawnNextEnemy - time < 0 && not dead
                                then
                                  let (pos, g')       = randomPos rndGen
                                  in w{ enemies         = Enemy pos : enemies,
                                        rndGen          = g',
                                        spawnNextEnemy  = 3.0 / (1.0 + timeAlive)}
                                else w{spawnNextEnemy = spawnNextEnemy - time}

tspawnBonus time w@World{..} = if spawnNextBonus == 0 && not dead
                                then
                                  let (pos,g')        = randomPos rndGen
                                  in w{ bonusses        = Bonus pos : bonusses,
                                        rndGen          = g',
                                        spawnNextBonus  = 150}
                              else w{spawnNextBonus   = spawnNextBonus - 1}

tspawnStars time w@World{..} = if spawnNextStar - time < 0
                                then w{ stars         = stars',
                                        rndGen        = g3',
                                        spawnNextStar = gen3}
                                else w{spawnNextStar  = spawnNextStar - time}
                              where (g1', gen1) = randomFloat 0.2 3.0 rndGen
                                    str         = Star (negate defaultHorizontalResolution/2 , gen2) gen1
                                    (g2', gen2) = randomFloat (negate defaultVerticalResolution /2) (1.5*defaultVerticalResolution) g1'
                                    (g3', gen3) = randomFloat 0 0.1 g2'
                                    stars'      = str : stars


tshoot time w@World{..} = let bullets' =  if cond
                                              then Bullet shiporientation shiplocation : bullets
                                              else bullets
                             in w{bullets = bullets', spawnNextBullet =  if cond then 0.05 else spawnNextBullet - time}
                                    where cond = shootAction == Shoot && spawnNextBullet - time < 0 && not dead -- Time based re-firing contr

-- Particle spawning

engineParticles                   :: Float -> World -> World
engineParticles time w@World{..}  = if not dead then w{particles = newparticle : particles, rndGen = g} else w
                                        where newparticle = Particle dir (update 10 dir shiplocation) (greyN 6) shps 0.35
                                              (dir,g)     = (shiporientation + snd random' + 180.0, fst random')
                                              random'     = randomFloat (-10) 10 rndGen
                                              shps = if movementAction == NoMovement
                                                       then 2.5
                                                       else 4.0

dyingParticles :: Point -> Color -> StdGen -> (StdGen, [Particle])
dyingParticles pos color rndGen = foldr newParticle (rndGen, [])  [0 .. 10]
                                where
                                  newParticle _ (g, ps) = (snd $ np g, fst (np g) : ps)
                                  np g = (Particle (snd $ dir g) pos color 1 0.5, fst $ dir g)
                                  dir = randomFloat 0 360

-- lifecycle managment

respawnChecker, tAliveUpdate :: Float -> World -> World

respawnChecker _ w@World{..} =  if dead && continue
                                  then  (\w -> w{stars = stars}) .
                                        initial $ truncate $ snd $
                                        randomFloat 0 100000 rndGen
                                  else w

tAliveUpdate time w@World{..} = if not dead
                                  then w{timeAlive = timeAlive + time}
                                  else w

-- Movement

tship, tenemies, tmoveStars, tparticles, tbullets, boundaryCleanup :: Float -> World -> World

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

tmoveStars time w@World{..} = let stars' = map (\(Star p s) -> Star (update s 116.57 p) s) stars
                              in w{stars = stars'}

tparticles time w@World{..} = w{particles = particles'}
                            where particles' = map updatePos $ filter p $ map (\p@Particle{..} -> p{dietime = dietime - time}) particles
                                  p Particle{..} = dietime > 0
                                  updatePos p@Particle{..} = p{position = update speed direction position}


tbullets  time w@World{..} = let bullets' = map (\(Bullet dir pos) -> Bullet dir (update 5 dir pos)) bullets
                              in w{bullets = bullets'}

boundaryCleanup _ w@World{..} = w{shiplocation  = checkShipVer $ checkShipHor shiplocation,
                                  particles     = particles',
                                  stars         = stars',
                                  bullets       = bullets'}
                            where checkShipHor p   | fst p < - dhr = (-dhr, snd p)
                                                   | fst p >   dhr = ( dhr, snd p)
                                                   | otherwise     = p
                                  checkShipVer p   | snd p < - dvr = (fst p, -dvr)
                                                   | snd p >   dvr = (fst p,  dvr)
                                                   | otherwise     = p
                                  particles'       = filter (not . (\Particle{..}   -> checkUpLeft position || checkDownRight position )) particles
                                  bullets'         = filter (not . (\(Bullet _ poz) -> checkUpLeft poz      || checkDownRight poz      )) bullets
                                  stars'           = filter (not . (\(Star poz _)   ->                         checkDownRight poz      )) stars
                                  checkUpLeft    p = fst p < -dhr ||
                                                     snd p >  dvr
                                  checkDownRight p = fst p >  dhr ||
                                                     snd p < -dvr
                                  dhr              = defaultHorizontalResolution / 2
                                  dvr              = defaultVerticalResolution   / 2


-- random generator functions

randomPos   :: StdGen -> (Point,StdGen)
randomPos g = let ((g',x),y) = (hor $ fst $ vert g, snd $ vert g)
              in ((x,y),g')
            where hor  = randomFloat (-defaultHorizontalResolution / 2) (defaultHorizontalResolution / 2)
                  vert = randomFloat (-defaultVerticalResolution   / 2) (defaultVerticalResolution   / 2)

randomFloat :: Float -> Float -> StdGen -> (StdGen, Float)
randomFloat lo hi g = let (x, g') = randomR (lo, hi) g
                    in (g',x)

--vector math

update                :: Float -> Float -> Point -> Point -- update a point with a polar vector
update speed dir pos  = let vec = (speed * (- cos dir'), speed * sin dir')
                        in vec <+> pos
                        where dir' = degToRad dir + (pi / 2)

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
