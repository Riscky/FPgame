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
timeHandler time w  = w >>= engineParticles >>= tparticles >>= tship >>= collisionHandler >>= tspawnEnemy >>= tspawnBonus >>= tenemies >>= tshoot >>= tbullets >>= updatetime
      where (>>=)           :: World -> (Float -> World -> World) -> World
            (>>=)      w' f = f timeslice w'
            updatetime t w' = w'{timeLastFrame = t}
            timeslice       = time - timeLastFrame w

collisionHandler   :: Float -> World -> World
collisionHandler _ = shipcoll . encoll . bonuscoll

shipcoll, shipcollEnemy, shipcollBonus,encoll, bonuscoll :: World -> World

shipcoll = shipcollEnemy . shipcollBonus
shipcollEnemy  w@World{..} = if dead
                          then initial (fst (random rndGen))
                          else w
                      where dead = any (\(Enemy y) -> boxCollision y shiplocation 40) enemies

shipcollBonus w@World{..} = if score > 0
                            then w{multiplier = multiplier + score, bonusses = filter (not . cond) bonusses}
                            else w
                          where score = length $ filter cond bonusses
                                cond (Bonus y) = boxCollision shiplocation y 40

encoll    w@World{..} = let enemies' = map Enemy $ multcoll enemies bullets unEnemy (\(Bullet _ x) -> x) 100
                        in  w{enemies = enemies', score = score + multiplier * (length enemies - length enemies')}

  -- let enemies' = filter p enemies
  --                       in w{enemies = enemies'}
  --                     where p (Enemy epos) = not (not (null bullets) &&
  --                                                             any (\(Bullet _ bpos) -> boxCollision bpos epos 10) bullets)

bonuscoll w@World{..} = let bonusses' = map Bonus $ multcoll bonusses bullets unBonus (\(Bullet _ x) -> x) 90
                        in  w{bonusses = bonusses'}

  -- let bonusses' = filter p bonusses
  --                       in w{bonusses = bonusses'}
  --                     where p (Bonus pos) = not (not (null bullets) &&
  --                                                             any (\(Bullet _ bpos) -> boxCollision bpos pos 10) bullets)

multcoll :: [a] -> [b] -> (a -> Point) -> (b -> Point) -> Float -> [Point]
multcoll as bs fa fb dist = filter p as'
                          where p a = not (not (null bs') &&
                                      any (\b -> boxCollision a b dist) bs')
                                bs' = map fb bs
                                as' = map fa as

tship, tenemies, tbullets, tshoot, tspawnEnemy, tspawnBonus, tparticles:: Float -> World -> World

tship     time w@World{..} = let (so,sl) = (shiporientation + ospeed,
                                            update speed shiporientation shiplocation)
                             in w{shiplocation = sl, shiporientation = so, angularVelocity = ospeed}
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

engineParticles :: Float -> World -> World
engineParticles time w@World{..}  = w{particles = newparticle : particles, rndGen = g}
                                        where newparticle = Particle dir shiplocation (greyN 6) 10 1
                                              (dir,g)     = (-shiporientation + snd random', fst random')
                                              random'     = randomFloat (-10) 10 rndGen

dyingParticles :: Float -> Point -> World -> World
dyingParticles time pos w@World{..}  = w{particles = fst nps ++ particles, rndGen = snd nps}
                                      where   nps                    = newparticles rndGen 100
                                              newparticles     :: StdGen -> Int -> ([Particle], StdGen)
                                              newparticles g 0 = ([], g)
                                              newparticles g n = (newparticle : fst np, snd np)
                                                                where np = newparticles g (n - 1)
                                              (newparticle,g')  = (Particle dir pos red 10 1, g)
                                              (g, dir)      = randomFloat 0 360 rndGen

tparticles time w@World{..} = w{particles = particles'}
                            where particles' = map updatePos $ filter p $ map (\p@Particle{..} -> p{dietime = dietime - time}) particles
                                  p Particle{..} = dietime > 0
                                  updatePos p@Particle{..} = p{position = update speed direction position}



tbullets  time w@World{..} = let bullets' = map (\(Bullet dir pos) -> Bullet dir (update 5 dir pos)) bullets
                              in w{bullets = bullets'}


tshoot    time w@World{..} = let bullets' =  if cond
                                              then Bullet shiporientation shiplocation : bullets
                                              else bullets
                             in w{bullets = bullets', spawnNextBullet =  if cond then 0.05 else spawnNextBullet - time}
                                    where cond = shootAction == Shoot && spawnNextBullet - time < 0  -- Time based re-firing contr

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
