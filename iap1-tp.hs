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
nombresDeUsuarios x = listaDeNombres (usuarios x)

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres ((_, a):as) = a : listaDeNombres as

-- describir qué hace la función: .....
--amigosDe :: RedSocial -> Usuario -> [Usuario]
--amigosDe = undefined

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (usuarios, relaciones, _) usuario = amigosDeRec relaciones []
  where
    amigosDeRec [] amigos = amigos
    amigosDeRec ((u1, u2):rs) amigos
      | u1 == usuario && pertenece u2 usuarios && not (pertenece u2 amigos) = amigosDeRec rs (u2:amigos)
      | u2 == usuario && pertenece u1 usuarios && not (pertenece u1 amigos) = amigosDeRec rs (u1:amigos)
      | otherwise = amigosDeRec rs amigos


-- Calcular la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = length (amigosDe red usuario)

-- Función principal para encontrar el usuario con más amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

-- Función auxiliar que compara recursivamente los usuarios para encontrar el que tiene más amigos
-- Si solo queda un usuario en la lista, devolvemos ese usuario
-- Si hay más de un usuario en la lista, comparamos el primero con el que tiene más amigos en el resto de la lista
usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux _ [u] = u
usuarioConMasAmigosAux red (u:us)
  | cantidadDeAmigos red u >= cantidadDeAmigos red usuarioConMasAmigosResto = u
  | otherwise = usuarioConMasAmigosResto
  where
    usuarioConMasAmigosResto = usuarioConMasAmigosAux red us

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


mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos a b | perteneceTodos a b = perteneceTodos b a
                    | otherwise = False

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
  | x == y    = True
  | otherwise = pertenece x ys

perteneceTodos :: Eq a => [a] -> [a] -> Bool
perteneceTodos [] _ = True
perteneceTodos (x:xs) ys = pertenece x ys && perteneceTodos xs ys


--Predicados Auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] l = True
mismosElementos (x:xs) l | pertenece x l && mismosElementos xs l = True
                         | otherwise = False

redSocialValida :: RedSocial -> Bool
redSocialValida red = usuariosValidos (usuarios red) && relacionesValidas (usuarios red) (relaciones red) && publicacionesValidas (usuarios red) (publicaciones red)

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | usuarioValido x && noHayIdsRepetidos (x:xs) = usuariosValidos xs
                       | otherwise = False 

usuarioValido :: Usuario -> Bool
usuarioValido u | idDeUsuario u > 0 && longitud (nombreDeUsuario u) > 0 = True  
                | otherwise = False 

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

--noHayIdsRepetidos. Dada una lista de usuarios, corrobora que no haya usuarios con Ids repetidos. La lógica esta en noIdRepetidoAux1 a partir de la comparacion de los Ids de dos usuarios.
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | noIdRepetidoAux2 x xs == True = noHayIdsRepetidos xs
                         | otherwise = False 
                         
--esta función es para comparar un usuario con una lista, más que nada para usarla en la funcion principal y quede más prolija la recursión.
noIdRepetidoAux2 :: Usuario -> [Usuario] -> Bool
noIdRepetidoAux2 u [] = True
noIdRepetidoAux2 u (x:xs) | noIdRepetidoAux1 u x == True = noIdRepetidoAux2 u xs
                          | otherwise = False 
                                                   
--Compara los Ids de dos usuarios. En caso de que sean distintos devuelve True.
noIdRepetidoAux1 :: Usuario -> Usuario -> Bool
noIdRepetidoAux1 u1 u2 | idDeUsuario u1 /= idDeUsuario u2 = True
                       | otherwise = False
                       
                       
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas u r = usuariosDeRelacionValidos u r && relacionesAsimetricas r && noHayRelacionesRepetidas r 

--usuariosDeRelacionValidos. Corrobora que en la lista de relaciones, cada usuario de la tupla dentro de la relacion se encuentre en la lista de usuarios y que además sean distintos entre si.  
--Es decir: Relacion = (usuario1, usuario2) => usuario1 y usuario2 deben pertenecer a [usuario], pero ademas usuario1 debe se distinto de usuario2.
--La logica en si se encuentra en la funcion usuariosRelacionValidosAux1
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos u [] = True
usuariosDeRelacionValidos u (x:xs) | usuarioRelacionValidosAux1 x u &&  usuariosDeRelacionValidos u xs = True
                                   | otherwise = False 

usuarioRelacionValidosAux1 :: Relacion -> [Usuario] -> Bool
usuarioRelacionValidosAux1 (u1,u2) r1 | pertenece u1 r1 && pertenece u2 r1 && u1 /= u2 = True
                                      | otherwise = False 


-- relacionesAsimetricas. Corrobora que en una lista de relaciones no se encuentra la misma tupla de usuarios pero invertidos. La lógica se encuentra en la función relacionAsimetricaAux.
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (x:xs) | relacionAsimetricaAux x xs = relacionesAsimetricas xs
                             | otherwise = False

-- relacionAsimetricaAux. Siendo Relacion = (usuario1,usuario2), corrobora que en la lista dada no haya una Relacion = (usuario2,usuario1). Esta función me sirve para la principal.
relacionAsimetricaAux :: Relacion -> [Relacion] -> Bool
relacionAsimetricaAux (u1,u2) r1 | pertenece (u2,u1) r1 = False
                                 | otherwise = True 

--noHayRelacionesRepetidas. Dada una lista de relaciones, corrobora que los Ids de los Usuarios correspondientes a cada Relacion sean distintos.La lógica esta en la funcion noRelacionRepAux1.
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (x:xs) | noRelacionRepAux2 x xs == True = noHayRelacionesRepetidas xs
                                | otherwise = False  
                                
--Es una función para complementar y hacer la recursión más prolija.
noRelacionRepAux2 :: Relacion -> [Relacion] -> Bool
noRelacionRepAux2 r1 [] = True
noRelacionRepAux2 r1 (x:xs) | noRelacionRepAux1 r1 x == True = noRelacionRepAux2 r1 xs
                            | otherwise = False
                            
--noRelacionRepAux1. Compara los Ids de dos Relaciones.
noRelacionRepAux1 :: Relacion -> Relacion -> Bool
noRelacionRepAux1 (u1,u2) (v1,v2) | noIdRepetidoAux1 u1 v1 && noIdRepetidoAux1 u2 v2 = True 
                                  | noIdRepetidoAux1 u1 v1 == False && noIdRepetidoAux1 u2 v2 == True = True
                                  | noIdRepetidoAux1 u1 v1 == True && noIdRepetidoAux1 u2 v2 == False = True
                                  | otherwise = False                                                          


publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pub = usuariosDePublicacionSonUsuariosDeRed us pub && usuariosDeLikeDePublicacionSonUsuariosDeRed us pub && noHayPublicacionesRepetidas pub

--usuariosDePublicacionSonUsuariosDeRed. De una lista de usuarios y una lista de publicaciones, se corrobora que los todos los usuarios dentro de la lista pertenezcan a las publicaciones.
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed u [] = True
-- una vez de que la lista este vacia, es porque todos los casos anteriores dieron True y no quedan más para comparar.
usuariosDePublicacionSonUsuariosDeRed u (x:xs) | pertenece (usuarioDePublicacion x) u && usuariosDePublicacionSonUsuariosDeRed u xs  = True 
--utilizo la funcion pertenece, en donde va a comparar el usuario de publicacion1 de la lista de publicaciones con la lista de usuarios (asi ve que pertenece). En caso de que pertenezca, sigue con la siguiente publicacion, en donde va a corroborar que el usuario de la publicacion2 esté en la lista de usuarios. Y así hasta que se vacie, en caso de que pertenezca; de lo contrario, da false y corta ahi.
                                               | otherwise = False                             

--usuariosDeLikeDePublicacionSonUsuariosDeRed. De una lista de usuarios y una lista de publicaciones, se corrobora que los usuarios que dieron likes a las publicaciones pertenezcan a la lista de usuarios.
usuariosDeLikeDePublicacionSonUsuariosDeRed:: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed u [] = True 
-- La lista de publicaciones queda vacia justamente si se cumple que los usuarios de TODAS las publicaciones que dan like corresponden a la lista de usuario. 
usuariosDeLikeDePublicacionSonUsuariosDeRed u (x:xs) | usuariosLikeValidos u (likesDePublicacion x) && usuariosDeLikeDePublicacionSonUsuariosDeRed u xs = True
--se llama a la funcion usuariosLikeValidos para que corrobore que los likes de la primera publicacion (head de [Publicacion]) están en la lista de usuarios. Y al mismo tiempo tiene que cumplirse en el resto de las publicaciones, por lo que se realiza la recursion llamando a la funcion pero en base a tail (xs).
                                                     | otherwise = False 

-- usuariosLikeValidos. La primer lista es de Usuarios y la segunda lista corresponde a los likes.
--La función corrobora que de los usuarios correspondientes a la lista de likes estén dentro de la lista de usuarios.
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos u [] = True 
usuariosLikeValidos u (x:xs) | pertenece x u && usuariosLikeValidos u xs = True
-- si el primer usuario pertenece a la lista de usuarios es True y se realiza la recursión sobre tail hasta vaciar la lista de likes. En caso de que en algún momento no se cumpla la condición, corta y tira False.
                             | otherwise = False 

-- noHayPublicacionesRepetidas, dada una lista de publicaciones, compara de que dos mismos usuarios no tengan la misma publicacion. La lógica en si está en la función noPubliRepAux1, esta es la que engloba al resto y justamente la que compara directo la lista de publis.
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas (x:xs) | noPubliRepAux2 x xs == True = noHayPublicacionesRepetidas xs
                                   | otherwise = False

--noPubliRepAux2. Sirve como auxiliar para comparar una publi con una lista de publis (es más que nada para meter noPubliRepAux1)
noPubliRepAux2 :: Publicacion -> [Publicacion] -> Bool
noPubliRepAux2 p1 [] = True
noPubliRepAux2 p1 (x:xs) | noPubliRepAux1 p1 x == True = noPubliRepAux2 p1 xs
                         | otherwise = False

-- noPubliRepAux1. Acá está lo interesante, al comparar dos publicaciones. En caso de que los usuarios sean iguales, la publicaciones (el String) debe ser distinto.
noPubliRepAux1 :: Publicacion -> Publicacion -> Bool
noPubliRepAux1 (u1,s1,v1) (u2,s2,v2) | noIdRepetidoAux1 u1 u2 == False = pubAux s1 s2 
-- para comparar los usuarios, lo que interesa son los Id asi que llamo a las funcion noIdRepetidoAux y comparo los usuarios. En caso de que sean iguales (es decir, noIdRepAux1 = False), las publicaciones (los string) deben ser distintos, para comparar esto llamo a la funcion pubAux.
                                     | otherwise = True

--pubAux. Compara las publicaciones. 
pubAux :: String -> String -> Bool
pubAux s1 s2 | s1 /= s2 = True 
               | otherwise = False 

--
-- CORREGIR?! cadenaDeAmigos. Aplicar relacionadosDirectos, pero en una lista de Usuarios. Entonces se compara el usuario1 con el usuario2,!!! (creo que queda fijo el usuario1, y no corre con el usuario2).
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] r = True
cadenaDeAmigos (x:xs) r = relacionadosDirectoAux x xs r

--CORREGIR?! Fija al primer usuario. Deberia realizarse la comparacion con todos.!!! 
relacionadosDirectoAux :: Usuario -> [Usuario] -> RedSocial -> Bool
relacionadosDirectoAux u [] r1 = True
relacionadosDirectoAux u (x:xs) r1 | relacionadosDirecto u x r1 && relacionadosDirectoAux u xs r1 = True
                                   | otherwise = False 

-- relacionadosDirecto. Corrobora que la lista de relaciones dentro de una RedSocial no se encuentren repetidas de forma inversa.
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 r | pertenece (u1,u2) (relaciones r) == True && pertenece (u2,u1) (relaciones r) == False = True 
                            | pertenece (u1,u2) (relaciones r) == False && pertenece (u2,u1) (relaciones r) == False = True 
                            | otherwise = False 

---

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon n (x:xs) | n == x = True
                    | otherwise = False

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon n [x] = True
terminaCon n (x:xs) | n /= x = terminaCon n xs
                    | otherwise = False

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs == False = sinRepetidos xs 
                    | otherwise = False 
