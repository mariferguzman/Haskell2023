-- Completar con los datos del grupo
--
-- Nombre de Grupo: sintaxError
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
-- Calcular la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = length (amigosDe red usuario)

-- Función principal para encontrar el usuario con más amigos
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
  where usuarioConMasAmigosResto = usuarioConMasAmigosAux red us
    

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r u = funcionAuxPublicacionesDe (publicaciones r) u 

-- funcionAux. Dada una lista de Publicaciones y el Usuario, obtengo una lista de Publicaciones pertenecientes a ese Usuario.

funcionAuxPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
funcionAuxPublicacionesDe [] u = []
funcionAuxPublicacionesDe (x:xs) u | usuarioDePublicacion x == u = x : funcionAuxPublicacionesDe xs u
                                   | otherwise = funcionAuxPublicacionesDe xs u    
--1.Tomo la primer publicacion de la lista.
--2.Comparo el usuario de la primer publicacion con el Usuario dado.
--3.Si son iguales, entonces lo quiero en la lista.
--Con la primera publicacion que suceda esto, sera la cabeza de la lista. Luego, tiene que proseguir con la siguiente publicacion. Para armar la lista, justamente utilizo la recursion.        
--En caso de que el usuario de la publicacion no coincida con el usuario dado, hay que pasar a la siguiente publicación y ver si ese coincide o no (y asi sucesivamente).

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAux (publicaciones r) u

publicacionesQueLeGustanAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAux [] u = []
publicacionesQueLeGustanAux (x:xs) u | pertenece u (likesDePublicacion x) = x : publicacionesQueLeGustanAux xs u
                                     | otherwise = publicacionesQueLeGustanAux xs u
--Se armó esta funcion como para prestarle atencion a la lista de publicaciones de la RedSocial y que este más ordenada la funcion... 
-- Se tiene la lista de Publicaciones de la RedSocial, tomo la primer publicacion (es decir, "x"), de ahí voy a querer mirar la lista de los usuarios que le dieron like (es decir, likeDePublicacion x).
-- Entonces, una vez que tengo esa lista con los likes me va a servir para ver si alguno de esos usuarios coincide con el Usuario dado (para esto utilizo la funcion pertenece).
--En caso de que el Usuario dado se encuentre dentro de la lista de los likes, entonces esa publicacion va a formar parte de la lista.
-- Con el primer caso que se cumpla esto, entonces sera la cabeza de la lista. De todas formas, debe continuar realizando la comparacion con la siguiente publicacion (aca se realiza la recursion).
-- En caso de que el usuario no se encuentre en la lista de likes, entonces tambien se debe pasar a la siguiente publicacion para seguir analizando hasta pasar por todas las publicaciones.

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2 


--Ejercicio 9: coloquialmente hablando, corrobora que exista algún un usuario2 (distinto del usuario de entrada) que le haya dado like a todas las publicaciones del usuario de entrada.
-- La logica esta en: publicacionesQueLeGustanA usuario2 = publicacionesDe usuarioDeEntrada; la cual se encuentra en la funcion "f1".

-- Otra manera de hacer tieneUnSeguidorFiel
seguidorfiel :: RedSocial -> Usuario -> Bool
seguidorfiel r u = f3 r u (publicacionesDe r u)

f3 :: RedSocial -> Usuario -> [Publicacion] -> Bool 
f3 r u p = f4 r u (likesDePublicacion (head p))

f4 :: RedSocial -> Usuario -> [Usuario] -> Bool
f4 r u [] = False
f4 r u (x:xs) | mismosElementos (publicacionesDe r u) (publicacionesQueLeGustanA r x) = True
              | otherwise = f4 r u xs

-- Lo que me interesa de aca es la lista de publicaciones perteneciente a la RedSocial y las publicaciones del Usuario dentro de esa RedSocial; van a ser utilizadas en las proximas funciones.
-- El resultado dependerá de las funciones siguientes.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u = f2 r (publicaciones r) (publicacionesDe r u)  


--f2: RedSocial -> [lista de publicaciones pertenecientes a la red social] -> [Publicaciones del usuario de entrada de la funcion "tieneUnSeguidorFiel"]
-- Tomo la primer publicación, de la cual lo que me interesa es la lista de los likes (así lo puedo ingresar en la funcion "f1"). 
-- Si en caso f1 pasa por todos los usuarios y queda vacia devolviendo False, entonces pasamos a la siguiente publicacion, de la cual se va a tomar nuevamente la lista de likes para poder introducirlo en la funcion "f1" (recursion).
-- En caso no se haya encontrado publicaciones que cumplan con lo pedido, entonces la lista quedará vacia, devolviendo False.
f2 :: RedSocial -> [Publicacion] -> [Publicacion] -> Bool 
f2 r [] p1 = False
f2 r (x:xs) p1 | f1 r (likesDePublicacion x) p1 = True
               | otherwise = f2 r xs p1

--f1: RedSocial -> [usuario que le dieron me gusta a publicacion] -> [Publicaciones del usuario de entrada de la funcion "tieneUnSeguidorFiel"]
-- Esa lista con los "likes", sirve para tomar el primer usuario de la lista y llamar a la funcion "publicacionesQueLeGustan"; de esta forma nos devolverá todas las publicaciones que le gusten a ese usuario y servira para realizar la comparacion.
-- Se compara la lista de publicaciones que le gustan a un usuario dentro de la lista de likes con la lista de publicaciones del usuario de entrada; para esto se utiliza la funcion "mismosElementos".
-- En caso de que sean iguales, entonces devuelve True. Y si no, pasa al siguiente usuario para repetir lo mismo.
-- Si la lista queda vacia, es porque justamente no se encontró ningún usuario que cumpla lo pedido; por lo tanto, ahi devuelve False. Y es acá donde se pasaria en la funcion "f2" a la siguiente publicacion.
f1 :: RedSocial -> [Usuario] -> [Publicacion] -> Bool
f1 r [] p1 = False
f1 r (y:ys) p1 | mismosElementos (publicacionesQueLeGustanA r y) p1 = True
               | otherwise = f1 r ys p1

-- describir qué hace la función: 
--deveulve el valor de verdad de una condicion dada por la union de tres condiciones que tienen que ser verdaderas
-- la lista de usuarios de la red tiene que ser mayor a >= 2, y esta lista debe empezar por u1 y terminar con u2
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 = longitud (usuarios r) >= 2
                                && empiezaCon u1 (usuarios r)
                                && terminaCon u2 (usuarios r) 


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
noHayIdsRepetidos (x:xs) | noIdRepetidoAux2 x xs = noHayIdsRepetidos xs
                         | otherwise = False 

noIdRepetidoAux2 :: Usuario -> [Usuario] -> Bool
noIdRepetidoAux2 u [] = True
noIdRepetidoAux2 u (x:xs) | noIdRepetidoAux1 u x = noIdRepetidoAux2 u xs
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
noHayRelacionesRepetidas (x:xs) | noRelacionRepAux2 x xs = noHayRelacionesRepetidas xs
                                | otherwise = False  

noRelacionRepAux2 :: Relacion -> [Relacion] -> Bool
noRelacionRepAux2 r1 [] = True
noRelacionRepAux2 r1 (x:xs) | noRelacionRepAux1 r1 x = noRelacionRepAux2 r1 xs
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
auxiliar2 u1 (x:xs) | auxiliar1 u1 x = True 
                    | otherwise = auxiliar2 u1 xs

auxiliar1 :: Usuario -> Publicacion -> Bool
auxiliar1 u1 (u2,s,v) = u1 == u2
                        
                                                  


noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas (x:xs) | noPubliRepAux2 x xs = noHayPublicacionesRepetidas xs
                                   | otherwise = False

noPubliRepAux2 :: Publicacion -> [Publicacion] -> Bool
noPubliRepAux2 p1 [] = True
noPubliRepAux2 p1 (x:xs) | noPubliRepAux1 p1 x = noPubliRepAux2 p1 xs
                         | otherwise = False

noPubliRepAux1 :: Publicacion -> Publicacion -> Bool
noPubliRepAux1 ((a,b),s1,u1) ((c,d),s2,u2) | a == c &&  b == d = funcion1 s1 s2 
                                           | a == c &&  b /= d = False 
                                           | otherwise = True

funcion1 :: String -> String -> Bool
funcion1 s1 s2 = s1 /= s2

--


---

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon n (x:xs) = n == x
                    

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon n [x] = True
terminaCon n (x:xs) | n /= x = terminaCon n xs
                    | otherwise = False

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | not (pertenece x xs) = sinRepetidos xs 
                    | otherwise = False 