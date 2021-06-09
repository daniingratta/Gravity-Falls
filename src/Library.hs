module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Criatura = Criatura {
    nivelDePeligrosidad :: Number
    ,puedeDeshacerseDeEllaCon :: Persona -> Bool 
} deriving (Show)

--SiempreDetras
siempredetras = Criatura 0 noTeDejaEnPaz

noTeDejaEnPaz :: Persona -> Bool
noTeDejaEnPaz _ = False

--Gnomo
gnomo = Criatura 0 (puedeDeshacerseDelGnomoSi "Soplador de Hojas")

grupoDeGnomos :: [Criatura] -> Criatura
grupoDeGnomos listaGnomos = Criatura (calculoNivelDePeligrosidadGnomos listaGnomos) (puedeDeshacerseDelGnomoSi "Soplador de Hojas")

calculoNivelDePeligrosidadGnomos:: [Criatura] -> Number
calculoNivelDePeligrosidadGnomos = (2^) . length 

puedeDeshacerseDelGnomoSi :: String -> Persona -> Bool
puedeDeshacerseDelGnomoSi objeto = (elem objeto) . items 

--Ejemplo para Probar en Consola
--listaGnomosEjemplo :: Number -> [Criatura]
--listaGnomosEjemplo cantGnomos = replicate cantGnomos gnomo

--Fantasma
type Requisito = Persona -> Bool

fantasma :: Number -> Requisito -> Criatura
fantasma categoria requisito = Criatura (calculoNivelDePeligrosidadFantasma categoria) requisito

calculoNivelDePeligrosidadFantasma :: Number -> Number
calculoNivelDePeligrosidadFantasma = (* 20)

--Persona
data Persona = Persona {
    edad :: Number
    ,items :: [String]
    ,cantExperiencia :: Number
} deriving (Show)

personaDePrueba = Persona 10 ["Soplador de Hojas"] 2

--Punto 2

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura | (puedeDeshacerseDeEllaCon criatura) persona = ganarExperiencia (cuantaExperienciaGana criatura) persona 
                                   | otherwise = seEscapa persona

type Item = String

contieneItemParaDeshacerse :: String -> Item -> Bool
contieneItemParaDeshacerse objeto = (== objeto) 

cuantaExperienciaGana :: Criatura -> Number
cuantaExperienciaGana = nivelDePeligrosidad

seEscapa :: Persona -> Persona
seEscapa = ganarExperiencia 1 

ganarExperiencia :: Number -> Persona -> Persona
ganarExperiencia numero persona = persona {cantExperiencia = (cantExperiencia persona) + numero } 

--Punto 3
cuantaExperienciaGanaluegoDelEnfrentamiento :: Persona -> [Criatura] -> Persona
cuantaExperienciaGanaluegoDelEnfrentamiento persona criaturas = foldl enfrentarCriatura persona criaturas

--Ejemplo De Consulta
listaGnomosEjemplo :: Number -> [Criatura]
listaGnomosEjemplo cantGnomos = replicate cantGnomos gnomo

requiereQue :: Persona -> Bool
requiereQue persona = tengaItem persona && tengaEdad persona    

tengaItem :: Persona -> Bool
tengaItem = (elem "Disfraz de Oveja") . items

tengaEdad :: Persona -> Bool
tengaEdad =  (<13) . edad 

tengaExperiencia :: Persona -> Bool
tengaExperiencia = (>10) . cantExperiencia

listaCriaturas = [siempredetras, grupoDeGnomos (listaGnomosEjemplo 10), fantasma 3 requiereQue, fantasma 1 tengaExperiencia]

--2da PARTE
--Punto 1

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf funcion condicion _ [ ] = [ ]
zipWithIf funcion condicion (x : xs) ( z : zs) | not (condicion z) = z : zipWithIf funcion condicion (x:xs) zs
                                               | otherwise = funcion x z : zipWithIf funcion condicion xs zs
--Punto 2a
abecedarioDesde :: Char -> [Char]
abecedarioDesde letra | letra >= 'a' && letra <= 'z' = [letra ..'z'] ++ ['a' .. letra]
                       | otherwise = ['a' .. 'z']

--Punto 2b
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraDesencriptar = ((flip retornarLetra ['a'..'z']) . (posicionDeLetraADesencriptar letraDesencriptar) . abecedarioDesde) letraClave

posicionDeLetraADesencriptar :: Char -> [Char] -> Number
posicionDeLetraADesencriptar letra (x:xs) | x /= letra = 1 + posicionDeLetraADesencriptar letra xs
                                          | otherwise = 0
retornarLetra :: Number -> [Char] -> Char
retornarLetra posicion = last . take (posicion+1) 

--Punto 2c
cesar :: Char -> [Char] -> String
cesar letraClave textoEncriptado = zipWithIf desencriptarLetra condicion (repeat letraClave) textoEncriptado

condicion :: Char -> Bool
condicion letra = elem letra ['a'..'z'] 

