import Distribution.SPDX (LicenseId(XSkat))
import System.Win32 (COORD(xPos))
-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined






-- Funciones 

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool 
mismosElementos n l |  longitudElementosAux n l == True && perteneceElementosAux n l == True = True
                    | otherwise = False  


longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

longitudElementosAux :: (Eq t) => [t] -> [t] -> Bool
longitudElementosAux x1 x2 | longitud x1 == longitud x2 = True
                           | otherwise = False 

perteneceElementosAux :: (Eq t) => [t] -> [t] -> Bool
perteneceElementosAux [] l = True
perteneceElementosAux (x:xs) l = pertenece x l && perteneceElementosAux xs l 

---

usuarioValido :: Usuario -> Bool
usuarioValido u | idDeUsuario u > 0 && longitud (nombreDeUsuario u) > 0 = True  
                | otherwise = False 

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | noIdRepetidoAux2 x xs == True = noHayIdsRepetidos xs
                         | otherwise = False 

noIdRepetidoAux2 :: Usuario -> [Usuario] -> Bool
noIdRepetidoAux2 u [] = True
noIdRepetidoAux2 u (x:xs) | noIdRepetidoAux1 u x == True = noIdRepetidoAux2 u xs
                          | otherwise = False 
                                                   

noIdRepetidoAux1 :: Usuario -> Usuario -> Bool
noIdRepetidoAux1 (u1,u2) (v1,v2) | u1 == v1 = False 
                                 | otherwise = True 

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | usuarioValido x && noHayIdsRepetidos (x:xs) = usuariosValidos xs
                       | otherwise = False 

---

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (x:xs) | noRelacionRepAux2 x xs == True = noHayRelacionesRepetidas xs
                                | otherwise = False  

noRelacionRepAux2 :: Relacion -> [Relacion] -> Bool
noRelacionRepAux2 r1 [] = True
noRelacionRepAux2 r1 (x:xs) | noRelacionRepAux1 r1 x == True = noRelacionRepAux2 r1 xs
                            | otherwise = False

noRelacionRepAux1 :: Relacion -> Relacion -> Bool
noRelacionRepAux1 ((a,b),(c,d)) ((e,f),(g,h)) | a /= e && c /= g = True
                                          | a /= e && c == g = funcionAux d h 
                                          | a == e && c /= g = funcionAux b f 
                                          | a == e && c == g = False 

funcionAux :: String -> String -> Bool
funcionAux d h | d == h = True
             | otherwise = False                                          

---

usuarioDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuarioDePublicacionSonUsuariosDeRed [] p1 = True
usuarioDePublicacionSonUsuariosDeRed (x:xs) p1 | auxiliar2 x p1 && usuarioDePublicacionSonUsuariosDeRed xs p1 = True 
                                               | otherwise = False 

auxiliar2 :: Usuario -> [Publicacion] -> Bool
auxiliar2 u1 [] = False 
auxiliar2 u1 (x:xs) | auxiliar1 u1 x == True = True 
                    | otherwise = auxiliar2 u1 xs

auxiliar1 :: Usuario -> Publicacion -> Bool
auxiliar1 u1 (u2,s,v) | u1 == u2 = True
                      | otherwise = False  
                                                  


noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas (x:xs) | noPubliRepAux2 x xs == True = noHayPublicacionesRepetidas xs
                                   | otherwise = False

noPubliRepAux2 :: Publicacion -> [Publicacion] -> Bool
noPubliRepAux2 p1 [] = True
noPubliRepAux2 p1 (x:xs) | noPubliRepAux1 p1 x == True = noPubliRepAux2 p1 xs
                         | otherwise = False

noPubliRepAux1 :: Publicacion -> Publicacion -> Bool
noPubliRepAux1 ((a,b),s1,u1) ((c,d),s2,u2) | a == c &&  b == d = funcion1 s1 s2 
                                           | a == c &&  b /= d = False 
                                           | otherwise = True

funcion1 :: String -> String -> Bool
funcion1 s1 s2 | s1 /= s2 = True
               | otherwise = False 

--


---

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon n (x:xs) | n == x = True
                    | otherwise = False
                    -- = n == x

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon n [x] = True
terminaCon n (x:xs) | n /= x = terminaCon n xs
                    | otherwise = False

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs == False = sinRepetidos xs 
                    | otherwise = False 

