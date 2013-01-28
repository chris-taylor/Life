{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

module Life where

import Control.Monad
import Control.Monad.Random
import Control.Comonad
import Control.Concurrent
import Data.Array
import Data.List as L

data Cell = O | I deriving (Eq,Ord,Show)

toc O = ' '
toc I = '#'

toi O = 0
toi I = 1

type Idx = (Int,Int)
type Bds = (Idx,Idx)

data Grid a = Grid Idx Bds (Array Idx a) deriving Show
type Life = Grid Cell

life i arr = Grid i (bounds arr) arr

instance Functor Grid where
    fmap f (Grid i b arr) = Grid i b (fmap f arr)

instance Comonad Grid where
    extract (Grid i _ arr) = arr ! i
    duplicate (Grid i bds arr) = Grid i bds $ listArray bds $ map (\j -> Grid j bds arr) (range bds)

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
        numLiveNbrs = sum $  map (toi . (arr!)) $ neighbours sz i

        ruleImpl I n | n < 2 || n > 3 = O -- live cell with fewer than 2/more than 3 live nbrs dies
                     | otherwise      = I -- otherwise it lives to the next generation
        ruleImpl O n | n == 3         = I -- dead cell with three live nbrs comes alive
                     | otherwise      = O -- otherwise continues to be dead

-- Grids --

update (Grid i b arr) xs = Grid i b (arr // map (,I) xs)
blank n = life (0,0) $ listArray ((0,0),(n,n)) (repeat O)
blinkerV (p,q) grid = update grid $ [(p-1,q), (p,q), (p+1,q)]
blinkerH (p,q) grid = update grid $ [(p,q-1), (p,q), (p,q+1)]
block (p,q) grid = update grid $ [(p,q), (p+1,q), (p,q+1), (p+1,q+1)]
glider (p,q) grid = update grid $ [(p,q), (p,q-1), (p,q-2), (p+1,q),(p+2,q-1)]

choose a b = do z <- getRandomR (0.0, 1.0 :: Float)
                return (if z < 0.5 then a else b)

randomGrid n = do cells <- replicateM ((n+1)*(n+1)) (choose I O)
                  return $ life (0,0) $ listArray ((0,0),(n,n)) cells

-- Run -- (e.g. runR 100 20)

runR n k = randomGrid k >>= run n

run n grid = when (n > 0) $ do putStrLn (showGrid grid)
                               putStrLn "---"
                               threadDelay delayTimeMicroSecs
                               run (n-1) (grid =>> rule)
            where
                delayTimeMicroSecs = round $ 1000000 * (1/fromIntegral frameRate)
                frameRate = 25

