module Colors where
  data ColorizeParams = ColorizeParams
    { spread :: Float
    , frequency :: Float
    , initial :: Float
    , background :: Bool
    }

  sinColor :: Float -> Int
  sinColor x = (round ((sin x) * 127)) + 128

  -- rainbow takes an angle (Float) and generates RGB (three ints).
  rainbow :: Float -> (Int, Int, Int)
  rainbow x = (sinColor x, sinColor $ x + (2 * pi / 3), sinColor $ x + (4 * pi / 3))

  colorize :: ColorizeParams -> String -> String
  colorize p (s:ss) =
    let
      esc = if (background p) then "48" else "38"
      (r, g, b) = rainbow $ initial p
    in "\x1b[" ++ esc ++ ";2;" ++ (show r) ++ ";" ++ (show g) ++ ";" ++ (show b) ++ ['m',s] ++ (colorize (p { initial = (initial p) + (frequency p) }) ss)
  colorize _ [] = "\x1b[0m"

  colorizeLines :: ColorizeParams -> [String] -> [String]
  colorizeLines p (l:ls) = (colorize p l):(colorizeLines (p { initial = (initial p) + (spread p) } ) ls)
  colorizeLines _ [] = []
