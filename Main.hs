{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.Random
import Data.Array
import System.Console.CmdArgs

import Life

data LifeOpts = LifeOpts { width :: Int
                         , height :: Int
                         , framerate :: Int
                         , numframes :: Int } deriving (Show,Data,Typeable)

myArgs = LifeOpts { width = def, height = def, framerate = def, numframes = def }

main = do args <- cmdArgs myArgs
          runRandom (numframes args) (height args) (width args) (framerate args)

runRandom n h w f = randomGrid h w >>= run n f

-- Grids --

update (Grid i b arr) xs = Grid i b (arr // map (,I) xs)
blank n = life (0,0) $ listArray ((0,0),(n,n)) (repeat O)
blinkerV (p,q) grid = update grid [(p-1,q), (p,q), (p+1,q)]
blinkerH (p,q) grid = update grid [(p,q-1), (p,q), (p,q+1)]
block (p,q) grid = update grid [(p,q), (p+1,q), (p,q+1), (p+1,q+1)]
glider (p,q) grid = update grid [(p,q), (p,q-1), (p,q-2), (p+1,q),(p+2,q-1)]

choose a b = do z <- getRandomR (0.0, 1.0 :: Float)
                return (if z < 0.5 then a else b)

randomGrid h w = do cells <- replicateM (h * w) (choose I O)
                    return $ life (0,0) $ listArray ((0,0),(h-1,w-1)) cells