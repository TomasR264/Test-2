module Lib where
import Text.Show.Functions

laVerdad = True


data Raton = Raton {nombre :: String,
                    edad :: Float,
                    peso :: Float,
                    enfermedades :: [String]
                    } deriving (Eq,Show)


cerebro :: Raton
cerebro = Raton {nombre = "Cerebro",
                 edad = 9,
                 peso = 0.2,
                 enfermedades = ["brucelosis", "sarampion", "tuberculosis"]
}


bicenterrata :: Raton
bicenterrata = Raton {nombre = "Bicenterrata",
                 edad = 256,
                 peso = 0.2,
                 enfermedades = []
}


huesudo :: Raton
huesudo = Raton {nombre = "Huesudo",
                 edad = 4,
                 peso = 10,
                 enfermedades = []
}


--Parte 2

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt (edad raton)}

hierbaVerde :: String -> Hierba 
hierbaVerde terminacion raton = raton {enfermedades = curarSegunTerminacion terminacion (enfermedades raton)}

curarSegunTerminacion :: String -> [String] -> [String]
curarSegunTerminacion terminacion listaDeEnfermedades = filter (not . (mismaTerminacion terminacion)) listaDeEnfermedades

mismaTerminacion :: String -> String -> Bool
mismaTerminacion terminacion enfermedad = terminacion == (losUltimos (length terminacion) enfermedad)

losUltimos :: Int -> [a] -> [a]
losUltimos c xs = drop (length xs - c) xs

alcachofa :: Hierba
alcachofa raton = raton {peso = quitarPeso (peso raton)}

quitarPeso :: Float -> Float
quitarPeso estePeso
 | estePeso > 2 = estePeso * 0.9
 | otherwise = estePeso * 0.95

 
hierbaZort :: Hierba 
hierbaZort raton = raton {nombre = "Pinky",
                 edad = 0,
                 peso = peso raton,
                 enfermedades = []
}


hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso = (max 0 (peso raton - 0.1)),enfermedades = quitarLasCortas (enfermedades raton)}

quitarLasCortas :: [String] -> [String]
quitarLasCortas  = filter ((>10) . length) 

type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = alcachofa.hierbaBuena.hierbaBuena.hierbaBuena

reduceFatFast :: Int -> Medicamento
reduceFatFast potencia raton = alcachofaPotenciada potencia (hierbaVerde "obesidad" raton)

alcachofaPotenciada :: Int -> Raton -> Raton
alcachofaPotenciada potencia raton = last $ take potencia (iterate alcachofa raton)

enfermedadesInfecciosas :: [String]
enfermedadesInfecciosas = [ "sis", "itis", "emia", "cocos"]


pdepCilina :: Medicamento
pdepCilina raton = foldr ($) raton  (map (hierbaVerde) enfermedadesInfecciosas)


--Punto 4

cantidadIdeal :: (Num a, Enum a) => (a -> Bool) -> a
cantidadIdeal condicion = head (filter condicion [1..])



lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = not(haySobrepeso medicamento ratones) && todosTienenMenosDeTresEnfermedades medicamento ratones

haySobrepeso :: Medicamento -> [Raton] -> Bool
haySobrepeso medicamento ratones = all (<2) (map peso (map medicamento ratones))

todosTienenMenosDeTresEnfermedades :: Medicamento -> [Raton] -> Bool
todosTienenMenosDeTresEnfermedades medicamento ratones = all ((<3).length) (map enfermedades (map medicamento ratones))


cantidadIdealReduceFatFast :: [Raton] -> Int
cantidadIdealReduceFatFast ratones = cantidadIdeal (\potencia -> lograEstabilizar (reduceFatFast potencia) ratones) 
