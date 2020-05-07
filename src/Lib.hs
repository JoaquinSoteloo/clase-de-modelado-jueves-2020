module Lib where

type Estatura = Int
type Hobbit = (String,Estatura)

bilbo :: Hobbit
bilbo = ("Bilbo Bolsonaro", 125)

rosita :: Hobbit
rosita = ("Rosita coto", 125)

merryl :: Hobbit
merryl = ("Meriadoc Dropdigamo",131)

pippin :: Hobbit
pippin = ("Peregrin Take",131)

reputacion :: Hobbit -> Int
reputacion hobbit = (length.fst) hobbit * (snd hobbit)

diferenciaEnEstatura :: Hobbit -> Hobbit -> Int
diferenciaEnEstatura (_,estatura) (_,otraEstatura) = estatura - otraEstatura
