{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (lines)


type Point a = (a, a)

data Line a = Line a a
  deriving (Show, Functor)

type Line' a = Line (Point a)


quadLines = concat
  [ [ Line (x*s, y*s)
           ((x+1)*s, y*s)
    , Line (x*s, y*s)
           (x*s, (y+1)*s)
    ]
  | x <- [0..30], y <- [0..40]
  ]
  where s = 160


triLines = concat
  [ [ Line (triPoint s x y)
           (triPoint s (x+1) y)
    , Line (triPoint s x y)
           (triPoint s x (y+1))
    , Line (triPoint s x y)
           (triPoint s (x+1) (y+1))
    ]
  | x <- [0..30], y <- [0..40]
  ]
  where s = 160


--hexLines = map (fmap (mapX (+s))) $ concat
hexLines = concat
  [ starLines s x y
  | x <- [-10..10], y <- [-10..10]
  ]
  where
    s = 100
    mapX f (x, y) = (f x, y)

starLines s x0 y0 = 
  [ Line (triPoint s x y)
         (triPoint s (x+1) y)
  , Line (triPoint s x y)
         (triPoint s x (y+1))
  , Line (triPoint s x y)
         (triPoint s (x-1) (y-1))
  ]
  where
    x = y0 + 2*x0 +1
    y = y0*2 + x0

triPoint :: Int -> Int -> Int -> Point Int
triPoint s x y = (x*s - div (y*s) 2, round $ fromIntegral y*h*fromIntegral s)
  where h = sqrt(3)/2


polyLines :: Int -> Int -> [Line' Int]
polyLines n r =
  [ Line (vert k) (vert (k+1))
  | k <- [0..n-1]
  ]
  where
    vert k = ( round $ fromIntegral r * cos (angle k)
             , round $ fromIntegral r * sin (angle k))
    angle k = fromIntegral k * 2*pi/(fromIntegral n)

translate dx dy (x, y) = (x+dx, y+dy)

poly5Lines = concat
   [ map (fmap $ translate (x*200-100) (y*200)) $ polyLines 5 100
   | x <- [1..3], y <- [1..4]
   ]

poly7Lines = concat
   [ map (fmap $ translate (x*200-100) (y*200)) $ polyLines 7 100
   | x <- [1..3], y <- [1..4]
   ]


lines = poly7Lines


--- svg stuff ---

svgLine :: Line' Int -> String
svgLine (Line (x1, y1) (x2, y2)) =
  "<line x1="++show x1++" y1="++show y1++" x2="++show x2++" y2="++show y2
  ++ " stroke='black'/>"

header = "<!DOCTYPE html>\n<html>\n<body>\n<svg width='1000' height='1000'>"
footer = "</svg>\n</body>\n</html>"

main = writeFile "foo.html" $ header ++ concatMap svgLine lines ++ footer
