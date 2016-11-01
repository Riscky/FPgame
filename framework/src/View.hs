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
    = pictures [ship world, dbullets bullets, denemies enemies]

ship :: World -> Picture
ship World{..} = uncurry Translate shiplocation .
                  Rotate shiporientation .
                  Color green $
                  Polygon [(8,-10),(0,10),(-8,-10)]

dbullets :: [Bullet] -> Picture
dbullets bullets = pictures (map dbullet bullets)
                 where dbullet (Bullet _ pos) = uncurry Translate pos .
                                                  Color yellow $
                                                  Circle 2

denemies :: [Enemy] -> Picture
denemies enemies = pictures (map denemy enemies)
                 where denemy (Enemy pos) = uncurry Translate pos .
                                              Color blue $
                                              Polygon[(10,0),(0,10),(-10,0),(0,-10)]
