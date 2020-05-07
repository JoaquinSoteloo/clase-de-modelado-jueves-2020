module Lib where

type Estatura = Int
type Fuerza = Int

type Hobbit = (String,Estatura,Fuerza,Bool)

nombre :: Hobbit -> String
nombre (nombre,_,_,_) = nombre

estatura :: Hobbit -> Estatura
estatura (_,estatura,_,_) = estatura

bilbo :: Hobbit
bilbo = ("Bilbo Bolsonaro", 125,30,True)

rosita :: Hobbit
rosita = ("Rosita coto", 125,40,True)

merryl :: Hobbit
merryl = ("Meriadoc Dropdigamo",131,50,True)

pippin :: Hobbit
pippin = ("Peregrin Take",131,50,True)

reputacion :: Hobbit -> Int
reputacion hobbit = (length.nombre) hobbit * (estatura hobbit)


diferenciaEnEstatura :: Hobbit -> Hobbit -> Int
diferenciaEnEstatura (_,estatura,_,_) (_,otraEstatura,_,_) = estatura - otraEstatura


capacidad :: Hobbit -> Int
capacidad (nombre,_,fuerza,True) = (length nombre) * 3
capacidad (nombre,estatura,_,False) = 10 + estatura