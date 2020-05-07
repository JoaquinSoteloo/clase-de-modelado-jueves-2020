module Lib where
import Text.Show.Functions
-- Bilbo Bolsonaro, qué es un jovit con 125 cm
-- Rosita Coto, qué es una jovit con 115 cm


type Fuerza = Int
data Jovit = UnJovit{nombre::String,
estatura::Int,
fuerza::Fuerza,
esDeLaCumbancha::Bool,
sortija::Sortija
} deriving Show

bilbo:: Jovit
bilbo = UnJovit "Bilbo Bolsonaro" 125 100 True sortijaDelOlvido

rosita:: Jovit
rosita = UnJovit "Rosita Coto" 115 120 True laSortijaDeZorbit

merryl:: Jovit
merryl = UnJovit "Meriadoc Brandigamo" 131  100  True laSortijaDeZorbit

pippin:: Jovit
pippin = UnJovit "Peregrin Took"  131 100 True laSortijaDeZorbit

-- reputación: longitud de su nombre multiplicado por su estatura
reputacion:: Jovit -> Int
reputacion jovit = estatura jovit * (length.nombre) jovit

-- diferencia de alturas
diferenciaAltura unJovit otroJovit = estatura unJovit - estatura otroJovit 
-- sin pattern matching lo hacen en sus casas!


--primera version que rompe porque tiene un error de tipos, ya que devuelve un string
--perderHabla1 :: Jovit -> Jovit
--perderHabla1 jovit = drop 4 (nombre jovit)


--perderHabla2 :: Jovit -> Jovit
--perderHabla2 (UnJovit nombre estatura fuerza esDeLaCumbancha) = UnJovit (drop 4 --nombre) estatura fuerza esDeLaCumbancha

perderHabla :: Jovit -> Jovit
perderHabla jovit = jovit{nombre = ((drop 4).nombre) jovit} 


inimputabilizar :: Jovit -> Jovit
inimputabilizar jovit = jovit{
    nombre = nombre jovit ++ "Sos inimputable",
    estatura = estatura jovit + 10,
    fuerza = div (fuerza jovit) 2,
    esDeLaCumbancha = True
}

type Efecto = (Jovit -> Jovit)
data Sortija = UnaSortija{
    nombreDeLaSortija::String,
    peso:: Int,
    inscripcion:: String,
    efecto:: Efecto
} deriving Show

laSortijaDeZorbit = UnaSortija "La sortija de zorbit" 3 "Una sortija para conquistarlos a todos" id


--inscripcionDeLaSortija :: Jovit -> String
--inscripcionDeLaSortija (UnJovit _ _ _ _ (UnaSortija _ _ inscripcion)) = inscripcion  

inscripcionDeLaSortija :: Jovit -> String
inscripcionDeLaSortija jovit = (inscripcion.sortija) jovit

sortijaDelOlvido :: Sortija
sortijaDelOlvido = UnaSortija "Una sortija del olvido" 13 "" perderHabla

sortijaDelaImpunidad :: Sortija
sortijaDelaImpunidad = UnaSortija "Sortija de la impunidad" 10 "Sos inimputable Jovit" inimputabilizar

aplicarEfecto :: Jovit -> Jovit
aplicarEfecto jovit = (efecto (sortija jovit)) jovit

aplicarEfecto' :: Jovit -> Jovit
aplicarEfecto' jovit = ((efecto.sortija) jovit) jovit
