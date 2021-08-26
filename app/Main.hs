module Main where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.Pure.Game

-- Window PArameters

nombre:: [Char]
ancho, alto, offset :: Int
ancho = 400
alto =  150
offset = 10 -- posicion de la ventana
nombre = "Math Escape"
-- MAin Window
m_window :: Display
m_window = InWindow nombre (ancho, alto) (offset, offset)

-- Loading assets
preload_bkg :: IO [Picture]
preload_bkg = do
        d_1 <-  loadBMP "assets/d_1.bmp"
        d_2 <-  loadBMP "assets/d_2.bmp"
        d_3 <-  loadBMP "assets/d_3.bmp"
        d_4 <-  loadBMP "assets/d_4.bmp"
        d_5 <-  loadBMP "assets/d_5.bmp"
        d_6 <-  loadBMP "assets/d_6.bmp"
        return [d_1, d_2, d_3, d_4, d_5, d_6]

-- Get a Random BackGround TODO: Hacer que bote elementos distintos en cada iteracion

rnd_bkg :: Picture
rnd_bkg = head preload_bkg

main :: IO ()
main = do
  --Load aLl Assets
        assets  <- preload
  --Display the Window
        display
          m_window
          white                    -- background color
          d_1

picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display
