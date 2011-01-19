{-# OPTIONS_GHC -O2 #-}

import Data.Array
import Data.List
import Data.Maybe

type WeightedGraph = [(Int,Int,Int)]
type DisArray = Array Int Int

inf = 10^9 :: Int

bellman_ford :: WeightedGraph -> Int -> Int -> Int
bellman_ford graph src target = d_finish ! target
  where
    n = length graph
    updateEdge d u v cost = if du < inf && du + cost < d ! v then Just (v, du + cost) else Nothing
      where
        du = d ! u
    update :: DisArray -> DisArray
    update d = d // [fromJust res | (x,y,c) <- graph, let res = updateEdge d x y c, isJust res]
    d_start :: DisArray
    d_start = array (0,n-1) [(i, if i == src then 0 else inf) | i <- [0..n-1]]
    d_finish :: DisArray
    d_finish = (iterate update d_start) !! n

problem_83 content = bellman_ford graph 0 (n * m - 1) + matrix_arr ! (0,0)
  where
    matrix :: [[Int]]
    matrix = map (\x -> read $ "[" ++ x ++ "]") . lines $ content
    n = length matrix
    m = length $ head matrix
    rang = ((0,0),(n-1,m-1))
    matrix_arr :: Array (Int,Int) Int
    matrix_arr = array rang [((x,y),c) | (x,ys) <- zip [0..] matrix, (y, c) <- zip [0..] ys]
    directs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    label (x,y) = x * m + y
    graph :: WeightedGraph
    graph = [ (label (x,y), label (nx,ny), matrix_arr ! (nx,ny))
            | (x,y) <- range rang
            , (dx,dy) <- directs
            , let nx = x + dx
            , let ny = y + dy
            , inRange rang (nx,ny)
            ]

main = readFile "input/matrix.txt" >>= print . problem_83
