module Main where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.UI.GLUT.Fonts
import Graphics.Gloss.Juicy

-- Window PArameters

nombre:: [Char]
ancho, alto, offset :: Int
ancho = 720
alto =  360
offset = 10 -- posicion de la ventana
nombre = "Math Escape"

fondoMain :: Color
fondoMain = black
-- MAin Window
m_window :: Display
m_window = InWindow nombre (ancho, alto) (offset, offset)

data Resultado
 = Bien
 | Mal
 | None
 deriving (Eq)

data Estado =
  Estado
  {
    nivel  :: Int
  , fond :: Int
  , ans    :: Resultado
  , buffer :: (Int, Int)
  }

-- type Nivel = ((Int, Int), Int)

rnd_bkg :: [Picture] -> Picture
rnd_bkg  x = head x

-- render :: GameState -> [Picture] -> Picture 
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

loadJPG :: [Char] -> IO Picture
loadJPG path = do
  aux <- loadJuicyJPG path
  case aux of
    Just p -> return p
    Nothing -> return Blank

loadPNG :: [Char] -> IO Picture
loadPNG path = do
  aux <- loadJuicyPNG path
  case aux of
    Just p -> return p
    Nothing -> return Blank

main :: IO ()
main = do
  --Load aLl Assets
        backgrounds  <- preload_bkg
        problems <- loadProblems
  --      test <- loadJPG "assets/1+2.jpg"
  --Display the Window
        play
          m_window
          fondoMain                    -- background color
          1                           -- bfps
          initialState
          (`renderLevel` (backgrounds, problems))
          inputHandler
          updateState

initialState :: Estado
initialState = Estado
  {
    nivel = 0 -- Max level 144
  , fond = 0
  , ans = None
  , buffer = (-1, 0)
  }
picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display

w_em :: Float -> Float
w_em x = x * (fromIntegral(ancho) / 100)

h_em :: Float -> Float
h_em x = x * (fromIntegral(alto) / 100)

-- type Nivel = ((Int, Int), Int) -> (Fondo, Numero de problema), 0 si esta esperando entrada, 1 si esta correcta la entrada de la respuesta, 2 si esta mal
-- | renderLevel renderiza el nivel con respescto al nivel o estado Actual

renderLevel :: Estado -> ([Picture], [(Picture, Int)]) -> Picture
renderLevel gs asets = pictures [fondo, problema, verificar]
    where
       verificar =  case (ans gs) of
         Bien -> fonds!!6
         Mal  -> fonds!!7
         None -> Blank --Text (show (nivel gs))--Blank -- TODO: verificar devera ser vinculado al asset correspondiente usando un case
       fondo = fonds!!(if nroF < 0  || nroF >= 144 then 8 else nroF)
       problema = if nroP < 0 || nroP >= (length problemas) then Blank else (Scale 0.5 0.5 $ (fst (problemas!!nroP)))
       fonds = fst asets
       problemas = snd asets
       nroP = nivel gs
       nroF = fond gs

-- | inputHandler controla los eventos de entrada por teclado del juego dado una entrada y el estado actual define un nuevo estado

inputHandler :: Event -> Estado -> Estado
inputHandler (EventKey (Char '2') Up _ _) gs = gs {buffer = (snd (buffer gs), 2)}
inputHandler (EventKey (Char '3') Up _ _) gs = gs {buffer = (snd (buffer gs), 3)}
inputHandler (EventKey (Char '4') Up _ _) gs = gs {buffer = (snd (buffer gs), 4)}
inputHandler (EventKey (Char '5') Up _ _) gs = gs {buffer = (snd (buffer gs), 5)}
inputHandler (EventKey (Char '6') Up _ _) gs = gs {buffer = (snd (buffer gs), 6)}
inputHandler (EventKey (Char '7') Up _ _) gs = gs {buffer = (snd (buffer gs), 7)}
inputHandler (EventKey (Char '8') Up _ _) gs = gs {buffer = (snd (buffer gs), 8)}
inputHandler (EventKey (Char '9') Up _ _) gs = gs {buffer = (snd (buffer gs), 9)}
inputHandler (EventKey (Char '0') Up _ _) gs = gs {buffer = (snd (buffer gs), 0)}
inputHandler (EventKey (Char '1') Up _ _) gs = gs {buffer = (snd (buffer gs), 1)}
inputHandler _ gs = gs

-- | updateState se encarga de actaulizar el nivel en cada ciclo (creo)

updateState :: Float -> Estado -> Estado
updateState _ gs =
  if (na /= -1) then
    gs {
      nivel = newN
    , fond = newFond
    , ans = newAns
    , buffer = newB
    }
  else
    gs {
        nivel = -1
      , fond = 8
      , ans = None
      , buffer = (1, 1)
       }
  where
    newN = if (ans gs) == Bien && na < 144 then ((na) + 1) else (if na >= 0 && na <144 then na else -1)
    newFond = (nivel gs) `mod` 6
    newAns = case (ans gs) of
      Bien -> None
      Mal -> None
      None -> if correcto then Bien else (if sw then None else Mal)
    newB = if (ans gs) == None then (buffer gs) else (if respuestasig > 9 then (-1, -1) else (-1, 0))
    numero = getNum (buffer gs)
    respuesta = (if na >= 0 then respuestas!!na else -1)
    respuestasig = respuestas!!((na) + 1)
    correcto = (numero == respuesta) && na /= (-1)
    sw = numero == (-1)
    na = nivel gs


getNum :: (Int, Int) -> Int
getNum x = if (fst x) == -1 then -1 else  (10 * (fst x)) + (snd x)

respuestas :: [Int]
respuestas = [
     2
  ,  3
  ,  4
  ,  5
  ,  6
  ,  7
  ,  8
  ,  9
  , 10
  ,  1
  ,  0
  ,  1

  ,  3
  ,  4
  ,  5
  ,  6
  ,  7
  ,  8
  ,  9
  , 10
  , 11
  ,  2
  ,  1
  ,  0
  ,  2

  ,  4
  ,  5
  ,  6
  ,  7
  ,  8
  ,  9
  , 10
  , 11
  , 12
  ,  3
  ,  2
  ,  1
  ,  0
  ,  3

  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 4
  , 3
  , 2
  , 1
  , 0
  ,  4

  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 5
  , 4
  , 3
  , 2
  , 1
  , 0
  ,  5

  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 6
  , 5
  , 4
  , 3
  , 2
  , 1
  , 0
  ,  6

  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 7
  , 6
  , 5
  , 4
  , 3
  , 2
  , 1
  , 0
  ,  7

  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 8
  , 7
  , 6
  , 5
  , 4
  , 3
  , 2
  , 1
  , 0
  ,  8

  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 9
  , 8
  , 7
  , 6
  , 5
  , 4
  , 3
  , 2
  , 1
  , 0
  , 9
  --Valor para evitar un overflow de la lista
  ,-1
  ,-1
  ]


  -- Loading assets
preload_bkg :: IO [Picture]
preload_bkg = do
        d_1 <-  loadJPG "assets/d_1.jpg"
        d_2 <-  loadJPG "assets/d_2.jpg"
        d_3 <-  loadJPG "assets/d_3.jpg"
        d_4 <-  loadJPG "assets/d_4.jpg"
        d_5 <-  loadJPG "assets/d_5.jpg"
        d_6 <-  loadJPG "assets/d_6.jpg"
        bien<-  loadPNG "assets/bien.png"
        mal <-  loadPNG "assets/mal.png"
        cre <-  loadJPG "assets/creditos.jpg"
        return [d_1, d_2, d_3, d_4, d_5, d_6, bien, mal, cre]

loadProblems :: IO [(Picture, Int)]
loadProblems = do
  p_1    <- loadJPG "assets/1+1.jpg"
  p_2    <- loadJPG "assets/1+2.jpg"
  p_3    <- loadJPG "assets/1+3.jpg"
  p_4    <- loadJPG "assets/1+4.jpg"
  p_5    <- loadJPG "assets/1+5.jpg"
  p_6    <- loadJPG "assets/1+6.jpg"
  p_7    <- loadJPG "assets/1+7.jpg"
  p_8    <- loadJPG "assets/1+8.jpg"
  p_9    <- loadJPG "assets/1+9.jpg"
  p_10   <- loadJPG "assets/1+0.jpg"
  p_11   <- loadJPG "assets/1-1.jpg"
  p_20   <- loadJPG "assets/1-0.jpg"
  p_21   <- loadJPG "assets/2+1.jpg"
  p_22   <- loadJPG "assets/2+2.jpg"
  p_23   <- loadJPG "assets/2+3.jpg"
  p_24   <- loadJPG "assets/2+4.jpg"
  p_25   <- loadJPG "assets/2+5.jpg"
  p_26   <- loadJPG "assets/2+6.jpg"
  p_27   <- loadJPG "assets/2+7.jpg"
  p_28   <- loadJPG "assets/2+8.jpg"
  p_29   <- loadJPG "assets/2+9.jpg"
  p_30   <- loadJPG "assets/2+0.jpg"
  p_31   <- loadJPG "assets/2-1.jpg"
  p_32   <- loadJPG "assets/2-2.jpg"
  p_40   <- loadJPG "assets/2-0.jpg"
  p_41   <- loadJPG "assets/3+1.jpg"
  p_42   <- loadJPG "assets/3+2.jpg"
  p_43   <- loadJPG "assets/3+3.jpg"
  p_44   <- loadJPG "assets/3+4.jpg"
  p_45   <- loadJPG "assets/3+5.jpg"
  p_46   <- loadJPG "assets/3+6.jpg"
  p_47   <- loadJPG "assets/3+7.jpg"
  p_48   <- loadJPG "assets/3+8.jpg"
  p_49   <- loadJPG "assets/3+9.jpg"
  p_50   <- loadJPG "assets/3+0.jpg"
  p_51   <- loadJPG "assets/3-1.jpg"
  p_52   <- loadJPG "assets/3-2.jpg"
  p_53   <- loadJPG "assets/3-3.jpg"
  p_60   <- loadJPG "assets/3-0.jpg"
  p_61   <- loadJPG "assets/4+1.jpg"
  p_62   <- loadJPG "assets/4+2.jpg"
  p_63   <- loadJPG "assets/4+3.jpg"
  p_64   <- loadJPG "assets/4+4.jpg"
  p_65   <- loadJPG "assets/4+5.jpg"
  p_66   <- loadJPG "assets/4+6.jpg"
  p_67   <- loadJPG "assets/4+7.jpg"
  p_68   <- loadJPG "assets/4+8.jpg"
  p_69   <- loadJPG "assets/4+9.jpg"
  p_70   <- loadJPG "assets/4+0.jpg"
  p_71   <- loadJPG "assets/4-1.jpg"
  p_72   <- loadJPG "assets/4-2.jpg"
  p_73   <- loadJPG "assets/4-3.jpg"
  p_74   <- loadJPG "assets/4-4.jpg"
  p_80   <- loadJPG "assets/4-0.jpg"
  p_81   <- loadJPG "assets/5+1.jpg"
  p_82   <- loadJPG "assets/5+2.jpg"
  p_83   <- loadJPG "assets/5+3.jpg"
  p_84   <- loadJPG "assets/5+4.jpg"
  p_85   <- loadJPG "assets/5+5.jpg"
  p_86   <- loadJPG "assets/5+6.jpg"
  p_87   <- loadJPG "assets/5+7.jpg"
  p_88   <- loadJPG "assets/5+8.jpg"
  p_89   <- loadJPG "assets/5+9.jpg"
  p_90   <- loadJPG "assets/5+0.jpg"
  p_91   <- loadJPG "assets/5-1.jpg"
  p_92   <- loadJPG "assets/5-2.jpg"
  p_93   <- loadJPG "assets/5-3.jpg"
  p_94   <- loadJPG "assets/5-4.jpg"
  p_95   <- loadJPG "assets/5-5.jpg"
  p_100   <- loadJPG "assets/5-0.jpg"
  p_101   <- loadJPG "assets/6+1.jpg"
  p_102   <- loadJPG "assets/6+2.jpg"
  p_103   <- loadJPG "assets/6+3.jpg"
  p_104   <- loadJPG "assets/6+4.jpg"
  p_105   <- loadJPG "assets/6+5.jpg"
  p_106   <- loadJPG "assets/6+6.jpg"
  p_107   <- loadJPG "assets/6+7.jpg"
  p_108   <- loadJPG "assets/6+8.jpg"
  p_109   <- loadJPG "assets/6+9.jpg"
  p_110   <- loadJPG "assets/6+0.jpg"
  p_111   <- loadJPG "assets/6-1.jpg"
  p_112   <- loadJPG "assets/6-2.jpg"
  p_113   <- loadJPG "assets/6-3.jpg"
  p_114   <- loadJPG "assets/6-4.jpg"
  p_115   <- loadJPG "assets/6-5.jpg"
  p_116   <- loadJPG "assets/6-6.jpg"
  p_120   <- loadJPG "assets/6-0.jpg"
  p_121   <- loadJPG "assets/7+1.jpg"
  p_122   <- loadJPG "assets/7+2.jpg"
  p_123   <- loadJPG "assets/7+3.jpg"
  p_124   <- loadJPG "assets/7+4.jpg"
  p_125   <- loadJPG "assets/7+5.jpg"
  p_126   <- loadJPG "assets/7+6.jpg"
  p_127   <- loadJPG "assets/7+7.jpg"
  p_128   <- loadJPG "assets/7+8.jpg"
  p_129   <- loadJPG "assets/7+9.jpg"
  p_130   <- loadJPG "assets/7+0.jpg"
  p_131   <- loadJPG "assets/7-1.jpg"
  p_132   <- loadJPG "assets/7-2.jpg"
  p_133   <- loadJPG "assets/7-3.jpg"
  p_134   <- loadJPG "assets/7-4.jpg"
  p_135   <- loadJPG "assets/7-5.jpg"
  p_136   <- loadJPG "assets/7-6.jpg"
  p_137   <- loadJPG "assets/7-7.jpg"
  p_140   <- loadJPG "assets/7-0.jpg"
  p_141   <- loadJPG "assets/8+1.jpg"
  p_142   <- loadJPG "assets/8+2.jpg"
  p_143   <- loadJPG "assets/8+3.jpg"
  p_144   <- loadJPG "assets/8+4.jpg"
  p_145   <- loadJPG "assets/8+5.jpg"
  p_146   <- loadJPG "assets/8+6.jpg"
  p_147   <- loadJPG "assets/8+7.jpg"
  p_148   <- loadJPG "assets/8+8.jpg"
  p_149   <- loadJPG "assets/8+9.jpg"
  p_150   <- loadJPG "assets/8+0.jpg"
  p_151   <- loadJPG "assets/8-1.jpg"
  p_152   <- loadJPG "assets/8-2.jpg"
  p_153   <- loadJPG "assets/8-3.jpg"
  p_154   <- loadJPG "assets/8-4.jpg"
  p_155   <- loadJPG "assets/8-5.jpg"
  p_156   <- loadJPG "assets/8-6.jpg"
  p_157   <- loadJPG "assets/8-7.jpg"
  p_158   <- loadJPG "assets/8-8.jpg"
  p_160   <- loadJPG "assets/8-0.jpg"
  p_161   <- loadJPG "assets/9+1.jpg"
  p_162   <- loadJPG "assets/9+2.jpg"
  p_163   <- loadJPG "assets/9+3.jpg"
  p_164   <- loadJPG "assets/9+4.jpg"
  p_165   <- loadJPG "assets/9+5.jpg"
  p_166   <- loadJPG "assets/9+6.jpg"
  p_167   <- loadJPG "assets/9+7.jpg"
  p_168   <- loadJPG "assets/9+8.jpg"
  p_169   <- loadJPG "assets/9+9.jpg"
  p_170   <- loadJPG "assets/9+0.jpg"
  p_171   <- loadJPG "assets/9-1.jpg"
  p_172   <- loadJPG "assets/9-2.jpg"
  p_173   <- loadJPG "assets/9-3.jpg"
  p_174   <- loadJPG "assets/9-4.jpg"
  p_175   <- loadJPG "assets/9-5.jpg"
  p_176   <- loadJPG "assets/9-6.jpg"
  p_177   <- loadJPG "assets/9-7.jpg"
  p_178   <- loadJPG "assets/9-8.jpg"
  p_179   <- loadJPG "assets/9-9.jpg"
  p_180   <- loadJPG "assets/9-0.jpg"

  return [
    (p_1    ,  2 ),
    (p_2    ,  3 ),
    (p_3    ,  4 ),
    (p_4    ,  5 ),
    (p_5    ,  6 ),
    (p_6    ,  7 ),
    (p_7    ,  8 ),
    (p_8    ,  9 ),
    (p_9    , 10 ),
    (p_10   ,  1 ),
    (p_11   ,  0 ),
    (p_20   ,  1 ),

    (p_21   ,  3 ),
    (p_22   ,  4 ),
    (p_23   ,  5 ),
    (p_24   ,  6 ),
    (p_25   ,  7 ),
    (p_26   ,  8 ),
    (p_27   ,  9 ),
    (p_28   , 10 ),
    (p_29   , 11 ),
    (p_30   ,  2 ),
    (p_31   ,  1 ),
    (p_32   ,  0 ),
    (p_40   ,  2 ),

    (p_41   ,  4 ),
    (p_42   ,  5 ),
    (p_43   ,  6 ),
    (p_44   ,  7 ),
    (p_45   ,  8 ),
    (p_46   ,  9 ),
    (p_47   , 10 ),
    (p_48   , 11 ),
    (p_49   , 12 ),
    (p_50   ,  3 ),
    (p_51   ,  2 ),
    (p_52   ,  1 ),
    (p_53   ,  0 ),
    (p_60   ,  3 ),

    (p_61   , 5  ),
    (p_62   , 6  ),
    (p_63   , 7  ),
    (p_64   , 8  ),
    (p_65   , 9  ),
    (p_66   , 10 ),
    (p_67   , 11 ),
    (p_68   , 12 ),
    (p_69   , 13 ),
    (p_70   , 4  ),
    (p_71   , 3  ),
    (p_72   , 2  ),
    (p_73   , 1  ),
    (p_74   , 0  ),
    (p_80   ,  4 ),

    (p_81   , 6   ),
    (p_82   , 7   ),
    (p_83   , 8   ),
    (p_84   , 9   ),
    (p_85   , 10   ),
    (p_86   , 11  ),
    (p_87   , 12  ),
    (p_88   , 13  ),
    (p_89   , 14  ),
    (p_90   , 5   ),
    (p_91   , 4   ),
    (p_92   , 3   ),
    (p_93   , 2   ),
    (p_94   , 1   ),
    (p_95   , 0  ),
    (p_100  ,  5  ),

    (p_101  , 7  ),
    (p_102  , 8  ),
    (p_103  , 9  ),
    (p_104  , 10  ),
    (p_105  , 11  ),
    (p_106  , 12 ),
    (p_107  , 13 ),
    (p_108  , 14 ),
    (p_109  , 15 ),
    (p_110  , 6  ),
    (p_111  , 5  ),
    (p_112  , 4  ),
    (p_113  , 3  ),
    (p_114  , 2  ),
    (p_115  , 1 ),
    (p_116  , 0 ),
    (p_120  ,  6 ),

    (p_121  , 8  ),
    (p_122  , 9  ),
    (p_123  , 10  ),
    (p_124  , 11 ),
    (p_125  , 12 ),
    (p_126  , 13 ),
    (p_127  , 14 ),
    (p_128  , 15 ),
    (p_129  , 16 ),
    (p_130  , 7  ),
    (p_131  , 6  ),
    (p_132  , 5  ),
    (p_133  , 4  ),
    (p_134  , 3  ),
    (p_135  , 2  ),
    (p_136  , 1  ),
    (p_137  , 0 ),
    (p_140  ,  7 ),

    (p_141  , 9  ),
    (p_142  , 10  ),
    (p_143  , 11  ),
    (p_144  , 12 ),
    (p_145  , 13 ),
    (p_146  , 14 ),
    (p_147  , 15 ),
    (p_148  , 16 ),
    (p_149  , 17 ),
    (p_150  , 8  ),
    (p_151  , 7  ),
    (p_152  , 6  ),
    (p_153  , 5  ),
    (p_154  , 4  ),
    (p_155  , 3  ),
    (p_156  , 2  ),
    (p_157  , 1 ),
    (p_158  , 0 ),
    (p_160  ,  8 ),

    (p_161  , 10  ),
    (p_162  , 11  ),
    (p_163  , 12  ),
    (p_164  , 13 ),
    (p_165  , 14 ),
    (p_166  , 15 ),
    (p_167  , 16 ),
    (p_168  , 17 ),
    (p_169  , 18 ),
    (p_170  , 9  ),
    (p_171  , 8  ),
    (p_172  , 7  ),
    (p_173  , 6  ),
    (p_174  , 5  ),
    (p_175  , 4  ),
    (p_176  , 3  ),
    (p_177  , 2 ),
    (p_178  , 1 ),
    (p_179  , 0 ),
    (p_180  , 9 )
    -- Relleno
    --(Blank , 0)

         ]
