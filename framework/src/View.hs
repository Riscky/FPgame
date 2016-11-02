{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@World{..}
    = pictures [dship world, dbullets bullets, denemies enemies, dbonusses bonusses, dscore score]

dship :: World -> Picture
dship World{..} = uncurry Translate shiplocation .
                  Rotate shiporientation .
                  Color red $
                  Polygon [(8,-10),(0,10),(-8,-10)]

dscore :: Int -> Picture
dscore s = uncurry Translate (100, 100) . color white $ Text (show s)

dbullets         :: [Bullet] -> Picture
dbullets bullets = pictures (map dbullet bullets)
                 where dbullet (Bullet _ pos) = uncurry Translate pos .
                                                  Color yellow $
                                                  circleSolid 4

denemies         :: [Enemy] -> Picture
denemies enemies = pictures (map denemy enemies)
                 where denemy (Enemy pos) = uncurry Translate pos .
                                              Color blue $
                                              Polygon[(10,0),(0,10),(-10,0),(0,-10)]

dbonusses          :: [Bonus] -> Picture
dbonusses bonusses = pictures (map dbonus bonusses)
                    where dbonus (Bonus pos) = uncurry Translate pos .
                                                  Color green $
                                                  circleSolid 8
