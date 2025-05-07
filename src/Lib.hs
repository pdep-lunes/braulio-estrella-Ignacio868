module Lib (bolaEspinosa, Personaje(UnPersonaje)) where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    vidaInicial :: Int,
    cantidadVida :: Int,
    estado :: String
} deriving (Show)

misPersonajes :: [Personaje]
misPersonajes = [UnPersonaje "Pamela" "Lluvia de tuercas sanadoras" "Torreta Curativa" False 9600 9600 "Vivo",
                  UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas" True 4500 4500 "Vivo",
                  UnPersonaje "El Primo" "Puño Drenaje" "Caida Especial" True 500 500 "Vivo"]

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje 
    | cantidadVida unPersonaje <= 1000 = unPersonaje {cantidadVida = 0}
    | otherwise = restoMilVida unPersonaje

restoMilVida :: Personaje -> Personaje
restoMilVida unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje - 1000} 

lluviaDeTuercas :: Bool -> Personaje -> Personaje
lluviaDeTuercas esSanadora unPersonaje 
    | esSanadora = unPersonaje {cantidadVida = cantidadVida unPersonaje + 800}
    | otherwise = unPersonaje {cantidadVida = cantidadVida unPersonaje `div` 2}

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio unPersonaje 
    | (radio >3) && (cantidadVida unPersonaje <800) = 
        unPersonaje {superPoder = superPoder unPersonaje ++ "Espina estuvo aqui", superPoderActivo = False, cantidadVida = 0}
    | (radio >3) = unPersonaje {superPoder = superPoder unPersonaje ++ "Espina estuvo aqui"}
    | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Personaje -> Personaje  
torretaCurativa unPersonaje = unPersonaje {cantidadVida = vidaInicial unPersonaje , superPoderActivo = True} 

atacarPoderEspecial :: Int -> Personaje -> Personaje -> Personaje
atacarPoderEspecial  preradio unPersonaje contrincante 
    | superPoderActivo unPersonaje = granadaDeEspinas preradio contrincante
    | otherwise = contrincante

braulersEnLasUltimas :: [Personaje] -> Int
braulersEnLasUltimas misPersonajes = (length.filter (\unPersonaje -> cantidadVida unPersonaje <800)) misPersonajes

