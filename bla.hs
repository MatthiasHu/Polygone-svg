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


lines = quadLines


--- svg stuff ---

svgLine :: Line Int -> String
svgLine (Line (x1, y1) (x2, y2)) =
  "<line x1="++show x1++" y1="++show y1++" x2="++show x2++" y2="++show y2
  ++ " stroke='black'/>"

header = "<!DOCTYPE html>\n<html>\n<body>\n<svg width='1000' height='1000'>"
footer = "</svg>\n</body>\n</html>"

main = writeFile "foo.html" $ header ++ concatMap svgLine lines ++ footer
