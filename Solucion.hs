module Solucion where
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
-- Itera sobre la lista de usuarios tomando de cada tupla el segundo valor de ella,
-- que por definicion es el nombre del usuario
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios x = listaDeNombres (usuarios x)

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres (a:as) = nombreDeUsuario a : listaDeNombres as


-- Ejercicio 2
-- Usa una funcion auxiliar amigosDeAux para iterar sobre las relaciones de la red,
-- si usuario se encuentra en la relacion, se agrega al usuario con el cual está relacionado a una lista 'amigos'
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red usuario = amigosDeAux (relaciones red)
  where
    amigosDeAux [] = []
    amigosDeAux ((u1, u2):rs)
      | u1 == usuario = u2 : amigosDeAux rs 
      | u2 == usuario = u1 : amigosDeAux rs
      | otherwise = amigosDeAux rs 

-- Ejercicio 3
-- Calcular la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = longitud (amigosDe red usuario)


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
-- Evalua si existe usuario con 10+ amigos, solo lo comprueba con el usuario de mas amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos r = cantidadDeAmigos r (usuarioConMasAmigos r) >= 10


-- Ejercicio 6
-- Llama a ´funcionAuxPublicacionesDe´ e ingresa en la función: las publicaciones dentro de la RedSocial y el Usuario de entrada.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r u = funcionAuxPublicacionesDe (publicaciones r) u 

--Tomo la primer publicacion de la lista.
--Comparo el usuario de la primer publicacion con el Usuario dado.
--Si son iguales, entonces lo quiero en el resultado(lista) y si no, no lo incluyo en la misma y paso a la siguiente publicacion.
--Con la primera publicacion que suceda esto, sera la cabeza de la lista. Luego, tiene que proseguir con la siguiente publicacion y asi sucesivamente. Para armar la lista, utilizo la recursion.        
--Va a recorrer por todas las publicaciones de la lista uno por uno hasta vaciarla; cuando sucede esto, termina la recursion y devuelve la lista con aquellas publicaciones que realizo el usuario.
funcionAuxPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
funcionAuxPublicacionesDe [] u = []
funcionAuxPublicacionesDe (x:xs) u | usuarioDePublicacion x == u = x : funcionAuxPublicacionesDe xs u
                                   | otherwise = funcionAuxPublicacionesDe xs u    


-- Ejercicio 7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAux (publicaciones r) u
-- Llama a ´publicacionesQueLeGustan´ e ingresa en la función: las publicaciones dentro de la RedSocial y el Usuario de entrada

--Tomo la primer publicacion de la lista.
--Corroboro que el Usuario dado pertenezca a la lista de likes de esa publicacion.
--Si se cumple esta condicion entonces lo quiero en el resultado(lista) y sera la cabeza de la misma; de lo contrario, no lo incluyo y paso a la siguiente publicacion.
--Con la primera publicacion que suceda esto, sera la cabeza de la lista. Luego, tiene que proseguir con la siguiente publicacion y volver a realizar el paso anterior. Para armar la lista, utilizo la recursion.
--Va a recorrer por todas las publicaciones de la lista uno por uno hasta vaciarla; cuando sucede esto, termina la recursion y devuelve la lista con aquellas publicaciones que el usuario le haya dado like.
publicacionesQueLeGustanAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAux [] u = []
publicacionesQueLeGustanAux (x:xs) u | pertenece u (likesDePublicacion x) = x : publicacionesQueLeGustanAux xs u
                                     | otherwise = publicacionesQueLeGustanAux xs u



-- Ejercicio 8
-- Corrobora que las publicaciones que le gustan al usuario1 sean iguales al usuario2, de ser asi devuelve True; en caso contrario, False.
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

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = longitud xs + 1

longitudElementosAux :: (Eq t) => [t] -> [t] -> Bool
longitudElementosAux x1 x2 | longitud x1 == longitud x2 = True
                           | otherwise = False 

perteneceElementosAux :: (Eq t) => [t] -> [t] -> Bool
perteneceElementosAux [] l = True
perteneceElementosAux (x:xs) l = pertenece x l && perteneceElementosAux xs l 

---
