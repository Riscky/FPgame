{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Picture
import Graphics.Gloss

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- ship
        shiplocation     :: Point,
        shiporientation  :: Float,
        angularVelocity  :: Float,
        -- Score
        multiplier       :: Int,
        score            :: Int,
        -- objects
        bullets          :: [Bullet],
        enemies          :: [Enemy],
        bonusses         :: [Bonus],
        -- particles and stars
        particles        :: [Particle],
        stars            :: [Star],
        -- Time
        timeLastFrame    :: Float,
        spawnNextEnemy   :: Float,
        spawnNextBonus   :: Int,
        spawnNextBullet  :: Float
    }

data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
                    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
                    deriving (Eq)
data Bullet         = Bullet Float  Point
newtype Enemy       = Enemy {unEnemy :: Point}
newtype Bonus       = Bonus {unBonus :: Point}
data Particle       = Particle {
                        direction :: Float,
                        position  :: Point,
                        color     :: Color,
                        speed     :: Float,
                        dietime   :: Float
                      }
data Star           = Star           Point       Float        -- position, speed

initial      :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot (0,0) 0 0 1 0 [] [] [] [] [] 0 10.0 0 0
