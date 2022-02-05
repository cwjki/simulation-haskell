module Main where

import Simulations (startSimulation)
import System.Random (mkStdGen)
import Utils (randomGen)


-- Params
n :: Int
n = 10

m :: Int
m = 9

t :: Int  -- the world changes every t units of time
t = 30

k :: Int -- total of turn of the simulation
k = 50

robots :: Int
robots = 6

kids :: Int
kids = 8

obstacles :: Int
obstacles = 6

dirt :: Int
dirt = 12

seed :: Int
seed = 45




main :: IO ()
main = do
    putStrLn ""
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn "| Proyecto de Simulación y Programación Declarativa, Juan Carlos Casteleiro Wong C411 |"
    putStrLn "---------------------------------------------------------------------------------------"
    putStrLn ""

    startSimulation n m robots kids obstacles dirt seed t k
