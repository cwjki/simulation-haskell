module Main where

import Simulations (simulate)
import System.Random (mkStdGen)
import Utils (randomGen)


-- Params
n :: Int
n = 10

m :: Int
m = 9

robots :: Int
robots = 5

kids :: Int
kids = 3

obstacles :: Int
obstacles = 1

dirt :: Int
dirt = 0

seed :: Int
seed = 49



main :: IO ()
main = do
    putStrLn ""
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn "| Proyecto de Simulación y Programación Declarativa, Juan Carlos Casteleiro Wong C411 |"
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn ""

    simulate n m robots kids obstacles dirt seed
