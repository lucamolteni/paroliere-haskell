module Paroliere where

import qualified Data.ByteString as B
import Data.Trie
import Data.Char
import Control.Monad
import Data.Array
import System.Random

word = toEnum . fromEnum

paroleLista :: IO [(B.ByteString, Int)]
paroleLista = do f <- B.readFile "../listaparole.txt"
                 let spl  = B.split (word '\n') f
                 return (map (\x -> (x, B.length x)) spl)

interactive = do putStrLn "Inserisci una parola:"
                 p <- B.getLine
                 t <- paroleTrie
                 let res = Data.Trie.lookup p t
                 case res of 
                   Just a -> putStrLn ("Trovata, la parola e' di " ++ (show a) ++ " caratteri.")
                   Nothing -> putStrLn "Non trovata"
                 return ()


main = do interactive
          
paroleTrie :: IO (Trie Int)
paroleTrie = do list <- paroleLista
                let l = fromList list
                return l

lettere :: IO (Array (Int, Int) Char)
lettere = do rand <- randGen
             return $ array ((1,1), (5,5)) (genLista rand)

height = 5
width = 5
totalNum = height * width
sizem  = (height, width)

genLista :: [Char] -> [((Int, Int), Char)]
genLista rannums = let coord = [(a,b) | a <- [1..height], b <- [1..width]] in
                   zip coord (take totalNum rannums)

randGen :: IO [Char]
randGen = do gen <- newStdGen
             return ((randomRs ('a', 'z') gen) :: [Char])

sibl = siblings sizem

siblings :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
siblings size coord 
         | x > width || y > height = error "Not in matrix"
         | x == 1 && y == 1 = (adj E) : (adj SE) : (adj S) : []
         | x == 1 && y == height = (adj N) : (adj NE) : (adj E) : []
         | x == width && y == 1 = (adj W) : (adj SW) : (adj S) : []
         | x == width && y == height = (adj N) : (adj NW) : (adj W) : []
         | x == width = (adj W) : (adj SW) : (adj S) : (adj NW) : (adj N) : []
         | x == 1 = (adj E) : (adj SE) : (adj S) : (adj NE) : (adj N) : []
         | y == height =  (adj N) : (adj NE) : (adj E) : (adj NW) : (adj W) : []
         | y == 1 =  (adj E) : (adj SE) : (adj S) : (adj SW) : (adj W) : []
         |  otherwise = (adj N) : (adj S) : (adj E) : (adj W) : (adj NE) : (adj NW) : (adj SE) : (adj SW) : []
         where 
           x = fst coord
           y = snd coord
           height = snd size
           width = fst size
           adj = getSibl coord

getSibl :: (Int, Int) -> Coord -> (Int, Int)
getSibl coord dir  = case dir of
                      N -> (x , y - 1)
                      S -> (x , y + 1)
                      E -> (x + 1, y)
                      W -> (x - 1, y)
                      NE -> (x + 1, y - 1)
                      NW -> (x - 1, y - 1)
                      SE -> (x + 1, y + 1)
                      SW -> (x - 1, y + 1)
                    where 
                            x = fst coord
                            y = snd coord

data Coord = N | S | E | W | NE | NW | SE | SW
