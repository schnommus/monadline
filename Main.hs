module Main where

import MonadLine
import Segments

main = do
    dirSeg <- directorySegments
    displaySegments ([userNameSegment] ++ dirSeg ++ [numJobsSegment] ++ [errorCodeSegment] ++ [bashSegment])
