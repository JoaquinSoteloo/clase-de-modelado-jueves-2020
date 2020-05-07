module Lib where

type Estatura = Int
type Fuerza = Int

data Hobbit = Hobbit String Estatura Fuerza Bool

nombre :: Hobbit -> String
nombre (Hobbit nombre _ _ _) = nombre

estatura :: Hobbit -> Estatura
estatura (Hobbit _ estatura _ _) = estatura

bilbo :: Hobbit
bilbo = Hobbit "Bilbo Bolsonaro" 125 30 True

rosita :: Hobbit
rosita = Hobbit "Rosita coto" 125 40 True 

merryl :: Hobbit
merryl = Hobbit "Meriadoc Dropdigamo" 131 50 True

pippin :: Hobbit
pippin = Hobbit "Peregrin Take" 131 50 True

reputacion :: Hobbit -> Int
reputacion hobbit = (length.nombre) hobbit * (estatura hobbit)


diferenciaEnEstatura :: Hobbit -> Hobbit -> Int
diferenciaEnEstatura (Hobbit _ estatura _ _) (Hobbit _ otraEstatura _ _) = estatura - otraEstatura


capacidad :: Hobbit -> Int
capacidad (Hobbit nombre _ fuerza True) = (length nombre) * 3
capacidad (Hobbit nombre estatura _ False) = 10 + estatura
