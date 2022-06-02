-- This software is in the public domain.
rainbow :: Float -> (Int, Int, Int)
rainbow x =
  ( (round ((sin $ 0.1 * x + 0.0) * 127)) + 128
  , (round ((sin $ 0.1 * x + 2.0) * 127)) + 128
  , (round ((sin $ 0.1 * x + 4.0) * 127)) + 128
  )

colorize :: Float -> String -> String
colorize f (s:ss) =
 let
    (r, g, b) = rainbow f
 in "\x1b[38;2;" ++ (show r) ++ ";" ++ (show g) ++ ";" ++ (show b) ++ ['m',s] ++ (colorize (f + 1) ss)
colorize _ [] = "\x1b[0m"

main :: IO ()
main = do
  reader <- getContents
  putStr $ colorize 0 reader
