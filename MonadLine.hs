module MonadLine where

import System.Console.ANSI
import System.Posix.User
import PowerlineCharacters
import Data.List.Split
import Data.List
import Control.Monad

data TerminalColor = TerminalColor { intensity :: ColorIntensity
                                   , color :: Color }
                     | Normal


data ColorSet      = ColorSet      { foregroundColor :: TerminalColor 
                                   , backgroundColor :: TerminalColor }

data Segment       = Segment       { contents :: IO String
                                   , terminator :: Char
                                   , colorSet :: ColorSet
                                   , bold :: Bool
                                   , transitionfg :: TerminalColor
                                   , displayWhen :: IO Bool }

alwaysDisplay :: IO Bool
alwaysDisplay = return True

neverDisplay :: IO Bool
neverDisplay = return True

sep = " "

beginNonPrintable = putStr "\\["
endNonPrintable = putStr "\\]"

setSGR_escaped args = do
    beginNonPrintable
    setSGR args
    endNonPrintable

resetColors :: IO ()
resetColors = setSGR_escaped [Reset]

setColors :: ColorSet -> IO ()
setColors (ColorSet Normal Normal) = resetColors
setColors (ColorSet ftcolor Normal) = do
    resetColors
    setSGR_escaped [SetColor Foreground (intensity ftcolor) (color ftcolor)]
setColors (ColorSet Normal btcolor) = do
    resetColors
    setSGR_escaped [SetColor Background (intensity btcolor) (color btcolor)]
setColors (ColorSet ftcolor btcolor) = do
    setSGR_escaped [SetColor Foreground (intensity ftcolor) (color ftcolor)]
    setSGR_escaped [SetColor Background (intensity btcolor) (color btcolor)]

setBold :: Bool -> IO ()
setBold True = setSGR_escaped[SetConsoleIntensity BoldIntensity]
setBold False = setSGR_escaped[SetConsoleIntensity NormalIntensity]

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

doDisplaySegments :: [Segment] -> IO ()
doDisplaySegments [segment] = displaySegment segment Normal
doDisplaySegments segments = do
    displaySegment (head segments) (backgroundColor.colorSet.head.tail$segments)
    doDisplaySegments (tail segments)

displaySegments :: [Segment] -> IO ()
displaySegments segments = filterM displayWhen segments >>= doDisplaySegments

replace old new = intercalate new . splitOn old
