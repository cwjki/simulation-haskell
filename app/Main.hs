module Main where

import Simulations (simulate)


-- Params
n :: Int
n = 10

m :: Int
m = 5

robots :: Int
robots = 2

kids :: Int
kids = 2

obstacles :: Int
obstacles = 2

dirt :: Int
dirt = 0

seed :: Int
seed = 50



main :: IO ()
main = do
    putStrLn ""
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn "| Proyecto de Simulación y Programación Declarativa, Juan Carlos Casteleiro Wong C411 |"
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn ""

    simulate n m robots kids obstacles dirt seed 