module Segments where

import MonadLine
import PowerlineCharacters
import System.Console.ANSI
import System.Posix.User
import System.Directory
import System.Environment
import Data.List.Split hiding (startsWith)
import Data.List
import Control.Applicative

-- Data we get from from bash command-line arguments
getErrorCode = (!! 0) <$> getArgs

getNumJobs = (!! 1) <$> getArgs

simpleSegment =    Segment
    { contents = return "simpleSegment"
    , terminator = right_arrow_hard
    , colorSet = ColorSet
      { foregroundColor = TerminalColor Vivid White
      , backgroundColor = TerminalColor Dull Blue
      }
    , bold = True
    , transitionfg = Normal
    , displayWhen = alwaysDisplay
    }

userNameSegment =    simpleSegment
    { contents = getEffectiveUserName
    }

bashSegment =    simpleSegment
    { contents = return "$"
    }

directorySegment dir =    Segment
    { contents = return dir
    , terminator = right_arrow_soft
    , colorSet = ColorSet
      { foregroundColor = TerminalColor Dull White
      , backgroundColor = TerminalColor Dull Black
      }
    , bold = False
    , transitionfg = TerminalColor Dull White
    , displayWhen = alwaysDisplay
    }

finalDirectorySegment dir =    Segment
    { contents = return dir
    , terminator = right_arrow_hard
    , colorSet = ColorSet
      { foregroundColor = TerminalColor Vivid White
      , backgroundColor = TerminalColor Dull Black
      }
    , bold = True
    , transitionfg = Normal
    , displayWhen = alwaysDisplay
    }

errorCodeSegment =    Segment
    { contents = getErrorCode
    , terminator = right_arrow_hard
    , colorSet = ColorSet
      { foregroundColor = TerminalColor Vivid White
      , backgroundColor = TerminalColor Dull Red
      }
    , bold = True
    , transitionfg = Normal
    , displayWhen = (/= "0") <$> getErrorCode
    }

numJobsSegment =    Segment
    { contents = getNumJobs
    , terminator = right_arrow_hard
    , colorSet = ColorSet
      { foregroundColor = TerminalColor Vivid White
      , backgroundColor = TerminalColor Dull Green
      }
    , bold = True
    , transitionfg = Normal
    , displayWhen = (/= "0") <$> getNumJobs
    }

-- Helper used in directorySegments for home directory replacing
replace old new = intercalate new . splitOn old

directorySegments :: IO [Segment]
directorySegments = do
    f <- getCurrentDirectory
    h <- getHomeDirectory
    let s = replace h "/~" f
    let l =            map
                (\d ->
                      if d == ""
                          then "/"
                          else d)
                (tail $ splitOn "/" s)
    return $
        (map directorySegment (init l)) ++ [finalDirectorySegment $ last l]

displayAll :: IO ()
displayAll = do
    dirSeg <- directorySegments
    displaySegments $
        [userNameSegment] ++
        dirSeg ++ [numJobsSegment, errorCodeSegment, bashSegment]
