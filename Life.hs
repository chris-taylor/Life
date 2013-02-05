module Life where

import Control.Monad
import Control.Comonad
import Control.Concurrent
import Data.Array
import Data.List as L

data Cell = O | I deriving (Eq,Ord,Show)

toc O = ' '
toc I = '#'

type Idx = (Int,Int)
type Bds = (Idx,Idx)

data Grid a = Grid Idx Bds (Array Idx a) deriving Show
type Life = Grid Cell

life i arr = Grid i (bounds arr) arr

instance Functor Grid where
    fmap f (Grid i b arr) = Grid i b (fmap f arr)

instance Comonad Grid where
    extract (Grid i _ arr) = arr ! i
    duplicate (Grid i bds arr) = Grid i bds $ listArray bds $ map (\j -> Grid j bds arr) $ range bds

cut n [] = []
cut n xs = take n xs : cut n (drop n xs)

showGrid (Grid _ ((_,b),(_,b')) arr) = L.intercalate "\n" $ cut (b'-b+1) $ map toc $ elems arr

-- Rules of Life --

wrap (m,n) (i,j) = (mod i (m+1), mod j (n+1))

neighbours :: Idx -> Idx -> [Idx]
neighbours sz (i,j) =
    [wrap sz (i+p,j+q) | p <- [-1,0,1], q <- [-1,0,1], (p,q) /= (0,0)]

rule :: Grid Cell -> Cell
rule (Grid i (_,sz) arr) = ruleImpl (arr!i) numLiveNbrs
    where
        numLiveNbrs = length $ filter (==I) $ map (arr!) $ neighbours sz i

        ruleImpl c n | n == 2    = c -- cell with 2 live nbrs stays the same
                     | n == 3    = I -- cell with 3 live nbrs comes alive (or remains alive)
                     | otherwise = O -- otherwise cell dies (or remains dead)

-- Run -- 

run n frameRate grid =
    when (n > 0) $ do putStrLn (replicate 10 '\n')
                      putStrLn (showGrid grid)
                      threadDelay delayTimeMicroSecs
                      run (n-1) frameRate (grid =>> rule)
                  where
                      delayTimeMicroSecs = round (1e6 / fromIntegral frameRate)

