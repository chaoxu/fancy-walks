{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Int
import Data.Ratio
import Data.Bits
import Data.Function
import Data.Ord
import Control.Monad.State
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Text.Parsec
import Text.Printf

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        content <- BS.unlines <$> replicateM n readLine
        m <- readInt
        query <- replicateM m $ do
            name <- readString
            k <- readInt
            replicateM k (BS.unpack <$> readString)
        return (content, query)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStr $ "Case #" ++ show cas ++ ": \n" ++ (solve params)

data PTree = Leaf Double | Branch Double String PTree PTree deriving Show

parseTree = do
    whitespace
    char '('
    whitespace
    weight <- read <$> (many1 $ oneOf $ ['0'..'9']++".")
    whitespace
    (char ')' >> return (Leaf weight)) <|> do 
        whitespace
        feature <- many1 $ oneOf ['a'..'z']
        whitespace
        lt <- parseTree
        whitespace
        rt <- parseTree
        whitespace
        char ')'
        return $ Branch weight feature lt rt
  where
    whitespace = many $ oneOf " \n\r\t"

solve (content, query) = unlines $ map (printf "%.10f".go tree) query
  where
    Right tree = parse parseTree "" content
    
    go (Leaf x) set = x
    go (Branch x str lt rt) set
        | elem str set = go lt set * x
        | otherwise    = go rt set * x
