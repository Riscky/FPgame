{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Picture

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
        -- objects
        bullets          :: [Bullet],
        enemies          :: [Enemy],
        bonusses         :: [Bonus],
        -- Time
        timeLastFrame    :: Float,
        spawnNextEnemy   :: Int,
        spawnNextBonus   :: Int
    }

data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
                    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
                    deriving (Eq)
data Bullet         = Bullet Float  Point
data Enemy          = Enemy         Point
data Bonus          = Bonus         Point

initial      :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot (0,0) 0 [] [] [] 0 0 0
