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

showLife (Grid _ bds arr) =
    let (a,b)   = fst bds
        (a',b') = snd bds
        es = elems arr
     in undefined

cut n [] = []
cut n xs = take n xs : cut n (drop n xs)

showGrid (Grid _ ((_,b),(_,b')) arr) = L.intercalate "\n" $ cut (b'-b+1) $ map toc $ elems arr

-- Rules of Life --

neighbours :: Idx -> Idx -> [Idx]
neighbours (m,n) (i,j) =
    map (\(a,b) -> (mod a (m+1), mod b (n+1))) $
        [(i+1,j), (i-1,j), (i,j+1), (i,j-1), (i+1,j+1), (i-1,j-1), (i+1,j-1), (i-1,j+1)]

rule :: Grid Cell -> Cell
rule (Grid i (_,sz) arr) = ruleImpl (arr!i) numLiveNbrs
    where
        numLiveNbrs = sum $  map (toi . (arr!)) $ neighbours sz i

        ruleImpl I n | n < 2 || n > 3 = O -- live cell with fewer than 2/more than 3 live nbrs dies
                     | otherwise      = I -- otherwise it lives to the next generation
        ruleImpl O n | n == 3         = I -- dead cell with three live nbrs comes alive
                     | otherwise      = O -- otherwise continues to be dead

runLife :: Grid Cell -> Grid Cell
runLife z = z =>> rule

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

-- Run --

frameRate :: Int
frameRate = 25

runR n k = do grid <- randomGrid k
              if n == 0
                then return ()
                else do putStrLn (showGrid grid)
                        putStrLn "---"
                        threadDelay delayTimeMicroSecs
                        run (n-1) (grid =>> rule)
            where
                delayTimeMicroSecs = round $ 1000000 * (1/fromIntegral frameRate)

run n grid = if n == 0
                then return ()
                else do putStrLn (showGrid grid)
                        putStrLn "---"
                        threadDelay delayTimeMicroSecs
                        run (n-1) (grid =>> rule)
            where
                delayTimeMicroSecs = round $ 1000000 * (1/fromIntegral frameRate)

