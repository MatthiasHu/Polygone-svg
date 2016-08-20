{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (lines)


type Point a = (a, a)

data Line a = Line (Point a) (Point a)
  deriving (Show, Functor)


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


triPoint :: Int -> Int -> Int -> Point Int
triPoint s x y = (x*s - div (y*s) 2, round $ fromIntegral y*h*fromIntegral s)
  where h = sqrt(3)/2


lines = triLines


--- svg stuff ---

svgLine :: Line Int -> String
svgLine (Line (x1, y1) (x2, y2)) =
  "<line x1="++show x1++" y1="++show y1++" x2="++show x2++" y2="++show y2
  ++ " stroke='black'/>"

header = "<!DOCTYPE html>\n<html>\n<body>\n<svg width='1000' height='1000'>"
footer = "</svg>\n</body>\n</html>"

main = writeFile "foo.html" $ header ++ concatMap svgLine lines ++ footer
