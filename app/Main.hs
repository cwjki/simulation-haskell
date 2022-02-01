module Main where

import Simulations (simulate)


-- Params
n :: Int
n = 10

m :: Int
m = 9

robots :: Int
robots = 2

kids :: Int
kids = 7

obstacles :: Int
obstacles = 5

dirt :: Int
dirt = 5

seed :: Int
seed = 5



main :: IO ()
main = do
    putStrLn ""
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn "| Proyecto de Simulación y Programación Declarativa, Juan Carlos Casteleiro Wong C411 |"
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn ""

    simulate n m robots kids obstacles dirt seed 