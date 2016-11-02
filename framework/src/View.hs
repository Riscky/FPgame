{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horRes vertRes world@World{..}
    = pictures [translated, static]
      where translated = pictures [dship world, dbullets bullets, denemies enemies, dbonusses bonusses]
            static = pictures [dscore score horRes vertRes, dmult multiplier horRes vertRes, dstars stars]



dship :: World -> Picture
dship World{..} = uncurry Translate shiplocation .
                  Rotate shiporientation .
                  Color red $
                  Polygon [(8,-10),(0,10),(-8,-10)]

dscore :: Int -> Float -> Float -> Picture
dscore s h v = uncurry Translate (h/2 - 124, v/2 - 30) .
                      Scale 0.2 0.2 .
                      Color white $ Text (show s)

dmult :: Int -> Float -> Float -> Picture
dmult s h v = uncurry Translate (h/2 - 224, v/2 - 30) .
                                    Scale 0.2 0.2 .
                                    Color white $ Text (show s ++ "x")

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

dstars         :: [Star] -> Picture
dstars = undefined
-- dstars stars = pictures (map drawStarAtLocation stars)
--     where drawStarAtLocation (Star p _ s) = uncurry Translate p .
--                                             Color white $
--                                             circleSolid s
