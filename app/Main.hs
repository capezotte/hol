module Main where
import qualified Colors
import System.Console.GetOpt
import System.Environment
import System.IO

-- TODO: read
-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- https://stackoverflow.com/questions/2346872/what-does-the-symbol-mean-in-haskell
-- again and make sure I actually get it
options :: [OptDescr (Colors.ColorizeParams -> Colors.ColorizeParams)]
options =
  [ (Option "s" ["spread"]
    (ReqArg (\arg f -> f { Colors.spread = (read arg :: Float) }) "ANGLE")
    "Spread (how much colors change vertically).")
  , (Option "f" ["frequency"]
    (ReqArg (\arg f -> f { Colors.frequency = (read arg :: Float) }) "ANGLE")
    "Frequency (how much colors change horizontally)." ) 
  , (Option "S" ["intial"]
    (ReqArg (\arg f -> f { Colors.initial = (read arg :: Float) }) "ANGLE")
    "Initial angle in the rainbow." ) 
  , (Option "i" ["background"]
    (NoArg (\f -> f { Colors.background = True }))
    "Whether to show colors in the background.")
  ]

argsProcess :: [String] -> IO ([Colors.ColorizeParams -> Colors.ColorizeParams], [String])
argsProcess argv =
  case getOpt Permute options argv of
    (flags, nonargs, []) -> return (flags, nonargs)
    (_, _, err)           -> ioError (userError $ concat err ++ usageInfo holUsage options)
  where holUsage = "Usage: hol [ OPTION... ] FILE..."

lolcatContents :: Colors.ColorizeParams -> FilePath -> IO ()
lolcatContents opts f = do 
  stdin <- if f == "-" then getContents else readFile f
  putStr $ unlines $ Colors.colorizeLines opts $ lines stdin

main :: IO ()
main = do
  (optacts,args) <- getArgs >>= argsProcess
  -- first class function abuse
  let opts = foldl (\acc x -> x acc) (Colors.ColorizeParams { Colors.spread = 0.2, Colors.frequency = 0.2, Colors.initial = 10, Colors.background = False }) optacts
  let files = if null args then ["-"] else args
  mapM_ (lolcatContents opts) files
