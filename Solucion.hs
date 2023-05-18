module Solution where
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

-- Ejercicio 1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios x = listaDeNombres (usuarios x)

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres ((_, a):as) = a : listaDeNombres as


-- Ejercicio 2
-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (usuarios, relaciones, _) usuario = amigosDeRec relaciones []
  where
    amigosDeRec [] amigos = amigos
    amigosDeRec ((u1, u2):rs) amigos
      | u1 == usuario && pertenece u2 usuarios && not (pertenece u2 amigos) = amigosDeRec rs (amigos ++ [u2])
      | u2 == usuario && pertenece u1 usuarios && not (pertenece u1 amigos) = amigosDeRec rs (amigos ++ [u1])
      | otherwise = amigosDeRec rs amigos


-- Ejercicio 3
-- Calcular la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = contarAmigos red usuario 0

-- Es una función auxiliar que realiza la recursión y utiliza un acumulador para contar los amigos
contarAmigos :: RedSocial -> Usuario -> Int -> Int
contarAmigos (_, relaciones, _) usuario acc = contarAmigosAux relaciones usuario acc

-- Esta función pasando el valor inicial del acumulador como 0.
-- Luego, la recursión se encarga de verificar las relaciones y aumentar el acumulador en caso de encontrar amigos válidos.
-- Al final, el valor del acumulador será la cantidad de amigos del usuario.
contarAmigosAux :: [Relacion] -> Usuario -> Int -> Int
contarAmigosAux [] _ acc = acc
contarAmigosAux ((u1, u2):rs) usuario acc
  | u1 == usuario = contarAmigosAux rs usuario (acc + 1)
  | u2 == usuario = contarAmigosAux rs usuario (acc + 1)
  | otherwise = contarAmigosAux rs usuario acc

-- Ejercicio 4
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
    

-- Ejercicio 5
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos r = auxRC r (usuarios r)

auxRC :: RedSocial -> [Usuario] -> Bool
auxRC r [] = False
auxRC r (u:us)
  | cantidadDeAmigos r u >= 10 = True
  | otherwise = auxRC r us


-- Ejercicio 6
-- funcionAux. Dada una lista de Publicaciones y el Usuario, obtengo una lista de Publicaciones pertenecientes a ese Usuario.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r u = funcionAuxPublicacionesDe (publicaciones r) u 

--1.Tomo la primer publicacion de la lista.
--2.Comparo el usuario de la primer publicacion con el Usuario dado.
--3.Si son iguales, entonces lo quiero en la lista.
--Con la primera publicacion que suceda esto, sera la cabeza de la lista. Luego, tiene que proseguir con la siguiente publicacion. Para armar la lista, justamente utilizo la recursion.        
--En caso de que el usuario de la publicacion no coincida con el usuario dado, hay que pasar a la siguiente publicación y ver si ese coincide o no (y asi sucesivamente).
funcionAuxPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
funcionAuxPublicacionesDe [] u = []
funcionAuxPublicacionesDe (x:xs) u | usuarioDePublicacion x == u = x : funcionAuxPublicacionesDe xs u
                                   | otherwise = funcionAuxPublicacionesDe xs u    


-- Ejercicio 7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAux (publicaciones r) u

--Se armó esta funcion como para prestarle atencion a la lista de publicaciones de la RedSocial y que este más ordenada la funcion... 
-- Se tiene la lista de Publicaciones de la RedSocial, tomo la primer publicacion (es decir, "x"), de ahí voy a querer mirar la lista de los usuarios que le dieron like (es decir, likeDePublicacion x).
-- Entonces, una vez que tengo esa lista con los likes me va a servir para ver si alguno de esos usuarios coincide con el Usuario dado (para esto utilizo la funcion pertenece).
-- En caso de que el Usuario dado se encuentre dentro de la lista de los likes, entonces esa publicacion va a formar parte de la lista.
-- Con el primer caso que se cumpla esto, entonces sera la cabeza de la lista. De todas formas, debe continuar realizando la comparacion con la siguiente publicacion (aca se realiza la recursion).
-- En caso de que el usuario no se encuentre en la lista de likes, entonces tambien se debe pasar a la siguiente publicacion para seguir analizando hasta pasar por todas las publicaciones.
publicacionesQueLeGustanAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAux [] u = []
publicacionesQueLeGustanAux (x:xs) u | pertenece u (likesDePublicacion x) = x : publicacionesQueLeGustanAux xs u
                                     | otherwise = publicacionesQueLeGustanAux xs u


-- Ejercicio 8
-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2 


--Ejercicio 9
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red usuario = seguidorFielAux red usuario (usuarios red)

-- En cada paso de la recursión, se examina un usuario de la lista. 
-- Si el usuario es diferente al usuario de entrada y le gustan todas las publicaciones del usuario de entrada, entonces tenemos un seguidor fiel. La función devuelve True en este caso.
-- Si el usuario no cumple con estas condiciones, se continúa con la recursión, pasando al siguiente usuario de la lista.
-- Si se llega al final de la lista sin encontrar un seguidor fiel, la función devuelve False.
seguidorFielAux :: RedSocial -> Usuario -> [Usuario] -> Bool
seguidorFielAux _ _ [] = False
seguidorFielAux red usuario (x:xs)
  | usuario /= x && todosElementosEn (publicacionesDe red usuario) (publicacionesQueLeGustanA red x) = True
  | otherwise = seguidorFielAux red usuario xs


-- Ejercicio 10
-- describir qué hace la función: 

-- Esta función verifica si existe una secuencia de amigos entre dos usuarios en la red social. 
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAux red u1 u2 []

-- Se llama a la función auxiliar existeSecuenciaDeAmigosAux pasando la red social, el primer usuario, el segundo usuario y una lista de usuarios visitados (inicialmente vacía).
-- Si el primer usuario es igual al segundo usuario, significa que se ha encontrado una secuencia de amigos, por lo que la función devuelve True.
-- Si el primer usuario ya ha sido visitado anteriormente (se encuentra en la lista de visitados), se evita la recursión para evitar bucles infinitos, y la función devuelve False.
-- En caso contrario, se realiza una llamada recursiva a la función existeSecuenciaDeAmigosAux para cada amigo del primer usuario. 
-- Se agrega el primer usuario actual a la lista de visitados. Si alguna de las llamadas recursivas devuelve True, significa que se encontró una secuencia de amigos, y la función devuelve True.
-- Si se recorren todos los amigos del primer usuario sin encontrar una secuencia de amigos, la función devuelve False.
existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario -> [Usuario] -> Bool
existeSecuenciaDeAmigosAux red u1 u2 visitados 
  | u1 == u2 = True
  | pertenece u1 visitados = False
  | otherwise = existe (\amigo -> existeSecuenciaDeAmigosAux red amigo u2 (u1:visitados)) (amigosDe red u1)


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


sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | not (pertenece x xs) = sinRepetidos xs 
                    | otherwise = False 

todosElementosEn :: Eq a => [a] -> [a] -> Bool
todosElementosEn [] _ = True
todosElementosEn (x:xs) ys = pertenece x ys && todosElementosEn xs ys

existe :: (a -> Bool) -> [a] -> Bool
existe _ [] = False
existe n (x:xs)
  | n x = True
  | otherwise = existe n xs