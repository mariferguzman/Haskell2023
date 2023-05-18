import Test.HUnit
import Solucion

main = runTestTT todosLosTests

todosLosTest = [testSuite1, testSuite2, testSuite3, testSuite4, testSuite5, testSuite6, testSuite7, testSuite8, testSuite9, testSuite10]

testSuite1 = test [" nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"]]

testSuite2 = test [" amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4]]

testSuite3 = test [" cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2]

testSuite4 = test [" usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4]]

testSuite5 = test [" estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False]

testSuite6 = test [
    " publicacionesDe 1 " ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
--Ejemplos propios
    " publicacionesDe devuelva lista vacia" ~: (publicacionesDe redC usuario6) ~?= []
    " publicacionesDe usuario con una publicacion" ~: (publicacionesDe redC usuario8) ~?= [publicacion8_1]
    " publicacionesDe usuario con más de una publicacion" ~: (publicacionesDe redC usuario7) ~?= [publicacion7_1, publicacion7_2]
 ]

testSuite7 = test [
    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
]

testSuite8 = test [
    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
]

testSuite9 = test [
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
]

testSuite10 = test [
    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
]




expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
--Ejemplos Propios
usuario6 = (6, "jorge")
usuario7 = (7, "mariela")
usuario8 = (8, "juan")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
--Ejemplos Propios
relacion6_7 = (usuario6, usuario7)
relacion6_8 = (usuario6, usuario8)
relacion7_8 = (usuario7, usuario8)


publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
--Ejemplos Propios
publicacion7_1 = (usuario7,"hi",[])
publicacion7_2 = (usuario7,"bye",[usuario6])

publicacion8_1 = (usuario8,"hey",[usuario7,usuario6])

--

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

--Ejemplos propios
usuariosC = [usuario6, usuario7, usuario8]
relacionesC = [relacion6_7, relacion6_8, relacion7_8]
publicaciones = [publicacion7_1, publicacion7_2, publicacion8_1]
redC = (usuariosC, relacionesC, publicacionesC)

