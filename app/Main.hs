module Main where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.UI.GLUT.Fonts


-- Window PArameters

nombre:: [Char]
ancho, alto, offset :: Int
ancho = 720
alto =  360
offset = 10 -- posicion de la ventana
nombre = "Math Escape"

fondo :: Color
fondo = black
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

rnd_bkg :: [Picture] -> Picture
rnd_bkg  x = head x

main :: IO ()
main = do
  --Load aLl Assets
        assets  <- preload_bkg
  --Display the Window
        display
          m_window
          fondo                    -- background color
          (pictures [head assets,  opciones])

picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display

w_em :: Float -> Float
w_em x = x * (fromIntegral(ancho) / 100)

h_em :: Float -> Float
h_em x = x * (fromIntegral(alto) / 100)

t_v_p  :: Float
t_v_p = 20

translate_arr :: [Picture -> Picture]
translate_arr = [Translate (w_em (-0)) (h_em (-10)),
                 Translate (w_em (t_v_p)) (h_em (-30)), Translate (w_em (t_v_p * (-1))) (h_em (-30)),
                 Translate (w_em (t_v_p)) (h_em (-60)), Translate (w_em (t_v_p * (-1))) (h_em (-60)) ]

translate_fun :: (Picture, Picture -> Picture) -> Picture
translate_fun x = (snd x) $ (fst x)

opciones :: Picture
opciones = pictures
  (map translate_fun e)
  where
    a = fst c
    b = snd c
    c = getQuizSuma 3 4
    d = ([parseQuiz a] ++ map parseQuiz b)
    e = zip d translate_arr


-- de string a rectangulo con texto para hacerle click

r_widht, r_height, r_s, r_t, r_font :: Float
r_fondo :: Color
r_widht = 115
r_height = 75
r_font = 30
r_fondo = white
r_s = 0.3
r_t = 0

parseQuiz :: String -> Picture
parseQuiz s = color red  (pictures
  [ color r_fondo $ translate r_t r_t $ rectangleSolid r_widht r_height
  , color blue    $ translate r_t r_t $ scale r_s r_s $ Text s
  ])


getQuizSuma :: Int -> Int -> ([Char], [[Char]])
getQuizSuma x y = (z, l)
                where
                  z = show (x + y)
                  a = concatS x "+" y
                  b = concatS (x + 1) "+" y
                  c = concatS x "+" (y + 1)
                  d = concatS (x - 1) "+" (y - 1)
                  l = [a, b, c, d]

sumaPic :: ([Char], [[Char]]) -> Picture
sumaPic x =
  Translate (-100) (-20)
  $Text  (fst x)

concatS :: Int -> [Char] -> Int -> [Char]
concatS x y z = show x ++ y ++ show z
