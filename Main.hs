module Main where

import System.Console.ANSI
import System.Posix.User
import PowerlineCharacters
import Data.List.Split
import Data.List
import System.Directory

data TerminalColor = TerminalColor { intensity :: ColorIntensity
                                   , color :: Color }
                     | Normal

data ColorSet = ColorSet { foregroundColor :: TerminalColor 
                         , backgroundColor :: TerminalColor
                         }

data Segment = Segment { contents :: IO String
                       , terminator :: Char
                       , colorSet :: ColorSet
                       , bold :: Bool
                       , transitionfg :: TerminalColor }

userNameSegment = Segment { contents = getEffectiveUserName
                          , terminator = right_arrow_hard 
                          , colorSet = ColorSet { foregroundColor = TerminalColor Vivid White
                                                , backgroundColor = TerminalColor Dull Blue }
                          , bold = True
                          , transitionfg = Normal}

bashSegment = userNameSegment { contents = return "$" }

directorySegment dir = Segment { contents = return dir
                               , terminator = right_arrow_soft
                               , colorSet = ColorSet { foregroundColor = TerminalColor Dull White
                                                     , backgroundColor = TerminalColor Dull Black }
                               , bold = False
                               , transitionfg = TerminalColor Dull White}

finalDirectorySegment dir = Segment { contents = return dir
                               , terminator = right_arrow_hard
                               , colorSet = ColorSet { foregroundColor = TerminalColor Vivid White
                                                     , backgroundColor = TerminalColor Dull Black }
                               , bold = True
                               , transitionfg = Normal }



sep = " "

resetColors :: IO ()
resetColors = setSGR [Reset]

setColors :: ColorSet -> IO ()
setColors (ColorSet Normal Normal) = resetColors
setColors (ColorSet ftcolor Normal) = do
    resetColors
    setSGR [SetColor Foreground (intensity ftcolor) (color ftcolor)]
setColors (ColorSet Normal btcolor) = do
    resetColors
    setSGR [SetColor Background (intensity btcolor) (color btcolor)]
setColors (ColorSet ftcolor btcolor) = do
    setSGR [SetColor Foreground (intensity ftcolor) (color ftcolor)]
    setSGR [SetColor Background (intensity btcolor) (color btcolor)]

setBold :: Bool -> IO ()
setBold True = setSGR[SetConsoleIntensity BoldIntensity]
setBold False = setSGR[SetConsoleIntensity NormalIntensity]

displaySegment :: Segment -> TerminalColor -> IO ()
displaySegment segment nextbg = do
    s <- contents segment
    setBold $ bold segment
    setColors $ colorSet segment
    putStr (sep ++ s ++ sep)
    resetColors
    setColors $ ColorSet 
        (case (transitionfg segment) of
            Normal -> (backgroundColor . colorSet $ segment)
            _ -> transitionfg segment)
        nextbg
    putStr [terminator segment]
    resetColors

displaySegments :: [Segment] -> IO ()
displaySegments [segment] = displaySegment segment Normal
displaySegments segments = do
    displaySegment (head segments) (backgroundColor.colorSet.head.tail$segments)
    displaySegments (tail segments)


replace old new = intercalate new . splitOn old

directorySegments :: IO [Segment]
directorySegments = do
    f <- getCurrentDirectory
    h <- getHomeDirectory
    let s = replace h "/~" f
    let l = tail $ splitOn "/" s
    return $ (map directorySegment (init l)) ++ [finalDirectorySegment $ last l]

main = do
    dirSeg <- directorySegments
    displaySegments ([userNameSegment] ++ dirSeg ++ [bashSegment])
