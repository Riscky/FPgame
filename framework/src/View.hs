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
                  Polygon [(8,0),(0,20),(-8,0)]

dbullets :: [Bullet] -> Picture
dbullets _ = blank

denemies :: [Enemy] -> Picture
denemies _ = blank
