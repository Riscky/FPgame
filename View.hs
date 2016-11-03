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
    = pictures [static, translated]
      where translated = pictures [dship world, dbullets bullets, denemies enemies, dbonusses bonusses, dparticle particles ]
            static = pictures [dscore score horRes vertRes, dmult multiplier horRes vertRes, dstars stars, dmoon, ddeadmsg dead world]



dship :: World -> Picture
dship World{..} | dead == False = uncurry Translate shiplocation $
                                            Rotate shiporientation $
                                            Pictures [ship, window, window']
                | otherwise     = Blank
                                  where ship = Color red $ Polygon [(2,-9),(8,-8),(4,-5),(4, 4),(0,10),(-4, 4),(-4,-5),(-8,-8),(-2,-9),(0,-12)]
                                        window = uncurry Translate (0,2) . Color white $ circleSolid 1.5
                                        window' =uncurry Translate (0,-4). Color white $ circleSolid 1.5

dscore :: Int -> Float -> Float -> Picture
dscore s h v = uncurry Translate (h/2 - 124, v/2 - 30) .
                      Scale 0.2 0.2 .
                      Color white $ Text (show s)

dmult :: Int -> Float -> Float -> Picture
dmult s h v = uncurry Translate (h/2 - 224, v/2 - 30) .
                                    Scale 0.2 0.2 .
                                    Color white $ Text (show s ++ "x")

dmoon :: Picture
dmoon = uncurry Translate (120, 60) $ Pictures [ele1, ele2, arcs, Pictures craters]
     where ele1 = Color (greyN 0.10) $ circleSolid 100
           ele2 = Color (makeColor 1   0   1   0.1) $ circleSolid 100
           arcs = Pictures $ map (arc') [1..7]
           arc' offset = Color (makeColor 0.7 0   1   0.25) $ thickArc (70+ 10*offset) (250-10*offset) (100-offset) (offset)
           craters = [crater (10, 7) 5,
                      crater (-30, -25) 7,
                      crater (50, -60) 10,
                      crater (-30, 60) 4]
           crater :: Point -> Float -> Picture
           crater pos size = uncurry Translate pos $ Pictures [Color (makeColor 0.5 0 0.7 0.1) $ circleSolid size, Rotate 180 . Color (makeColor 0.7 0 1 0.8) $ arc 70 250 size]



ddeadmsg :: Bool -> World -> Picture
ddeadmsg b w@World{..} = if b then Pictures [base, time] else Blank
                      where base = uncurry Translate (-250, 0) . Scale 0.2 0.2 . Color red $ Text "You have died! Press Enter to continue"
                            time = Blank

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

dparticle         :: [Particle] -> Picture
dparticle ps = pictures (map drawParticle ps)
          where drawParticle p@Particle{..} = uncurry Translate position .
                                              Color color $
                                              circleSolid $ 5.0 *dietime

dstars         :: [Star] -> Picture
dstars stars = pictures (map drawStarAtLocation stars)
    where drawStarAtLocation (Star p s) = uncurry Translate p .
                                            Color white $
                                            circleSolid s
