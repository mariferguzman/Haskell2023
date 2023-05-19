import Test.HUnit
import Solucion

main = runTestTT todosLosTest
todosLosTest = test [testSuite1, testSuite2, testSuite3, testSuite4, testSuite5, testSuite6, testSuite7, testSuite10]

run1 = runTestTT testSuite1
run2 = runTestTT testSuite2
run3 = runTestTT testSuite3
run4 = runTestTT testSuite4
run5 = runTestTT testSuite5
run6 = runTestTT testSuite6
run7 = runTestTT testSuite7
run8 = runTestTT testSuite8
run9 = runTestTT testSuite9
run10 = runTestTT testSuite10

-- NombresDeUsuarios
testSuite1 = test [
    "Caso 1: RedSocial vacía" ~: nombresDeUsuarios ([], [], []) ~?= [],
    "Caso 2: Un usuario" ~: nombresDeUsuarios ([(1, "Usuario1")], [], []) ~?= ["Usuario1"],
    "Caso 3: Varios usuarios, sin repetidos" ~: nombresDeUsuarios ([(1, "Usuario1"), (2, "Usuario2"), (3, "Usuario3")], [], []) ~?= ["Usuario1", "Usuario2", "Usuario3"]
    ]

-- AmigosDe
testSuite2 = test [
    "Caso 1: Usuario con un amigo" ~: amigosDe redB usuario3 ~?= [usuario2],
    "Caso 2: Usuario sin amigos" ~: amigosDe redB usuario4 ~?= []
    ]

-- cantidadDEaAmigos
testSuite3 = test [
    " Caso 1: Usuario con 3 amigos" ~: (cantidadDeAmigos redA usuario4) ~?= 3,
    " Caso 2: Usuario sin amigos" ~: (cantidadDeAmigos redA usuario5) ~?= 0,
    " Caso 2: Usuario con mas amigos" ~: (cantidadDeAmigos redC usuario12) ~?= 11
    ]

-- cantidadDEaAmigos
testSuite4 = test [
    " Caso 1: Usuario con mas Amigos" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4, usuario11]
    ]

--estaRobertoCarlos
testSuite5 = test [
    " El famoso Roberto Carlos esta! " ~: (estaRobertoCarlos redC) ~?= True,
    " El famoso Roberto Carlos no esta..." ~: (estaRobertoCarlos redA) ~?= False
    ]

-- publicacionesDe
testSuite6 = test [
    " publicacionesDe usuario con ninguna publicacion" ~: (publicacionesDe redD usuario5) ~?= [],
    " publicacionesDe usuario con una publicacion" ~: (publicacionesDe redE usuario1) ~?= [publicacion1_3],
    " publicacionesDe usuario con más de una publicacion" ~: (publicacionesDe redB usuario1) ~?= [publicacion1_3, publicacion1_4, publicacion1_5]
    ]

--publicacionesQueLeGustanA
testSuite7 = test [
    " publicacionesQueLeGustanA usuario que no likee ninguna publicacion " ~: (publicacionesQueLeGustanA redF usuario1) ~?= [],
    " publicacionesQueLeGustanA usuario likee exactamente una sola publicacion" ~: (publicacionesQueLeGustanA redG usuario2) ~?= [publicacion1_1],
    " publicacionesQueLeGustanA usuario likee más de una publicacion de un mismo usuario" ~: (publicacionesQueLeGustanA redD usuario2) ~?= [publicacion1_1, publicacion1_3],
    " publicacionesQueLeGustanA usuario likee exactamente una publicacion de dos usuarios distintos" ~: (publicacionesQueLeGustanA redD usuario4) ~?= [publicacion1_1,publicacion2_2],
    " publicacionesQueLeGustanA usuario likee varias publicaciones de distintos usuarios" ~: (publicacionesQueLeGustanA redA usuario4) ~?= [publicacion1_1, publicacion1_2, publicacion2_1,publicacion2_2]
    ]

-- lesGustanLasMismasPublicaciones    
testSuite8 = test [
    " No le gustan las mismas publicaciones " ~: (lesGustanLasMismasPublicaciones redC usuario4 usuario10) ~?= False
    ]

-- tieneUnSeguidorFiel    
testSuite9 = test [
    " No tiene un seguidor fiel " ~: (tieneUnSeguidorFiel redC usuario2 ) ~?= False
    ]

-- existeSecuenciaDeAmigos   
testSuite10 = test [
    " Existe secuencia de amigos entre usuario1 y usuario3 " ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    " Existe secuencia de amigos entre usuario1 y usuario5 " ~: (existeSecuenciaDeAmigos redA usuario1 usuario5) ~?= False,
    " Existe secuencia de amigos entre usuario4 y usuario3 " ~: (existeSecuenciaDeAmigos redA usuario4 usuario5) ~?= False
    ]
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
--Ejemplos Propios
usuario6 = (6, "Jorge")
usuario7 = (7, "Rosa")
usuario8 = (8, "Facundo")
usuario9 = (9, "Carlos")
usuario10 = (10, "Bryan")
usuario11 = (11, "Vivian")
usuario12 = (12, "Pikachu")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
--Ejemplos Propios
relacion1_12 = (usuario1, usuario12)
relacion2_12 = (usuario2, usuario12)
relacion3_12 = (usuario3, usuario12)
relacion4_12 = (usuario4, usuario12)
relacion5_12 = (usuario5, usuario12)
relacion6_12 = (usuario6, usuario12)
relacion7_12 = (usuario7, usuario12)
relacion8_12 = (usuario8, usuario12)
relacion9_12 = (usuario9, usuario12)
relacion10_12 = (usuario10, usuario12)
relacion11_12 = (usuario11, usuario12)
relacion6_7 = (usuario6, usuario7)

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
publicacion1_11 = (usuario2, "Hello World", [usuario1, usuario11])
publicacion2_11 = (usuario2, "Hello World", [usuario2, usuario11])
publicacion3_11 = (usuario2, "Hello World", [usuario3, usuario11])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

--Ejemplos propios
usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesC = [relacion1_12, relacion2_12, relacion3_12, relacion4_12, relacion5_12, relacion6_12, relacion7_12, relacion8_12,relacion9_12,relacion10_12,relacion11_12]
publicacionesC = [publicacion1_11, publicacion2_11, publicacion3_11]
redC = (usuariosC, relacionesC, publicacionesC)
--
usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesD = [relacion3_4, relacion1_3, relacion2_4]
publicacionesD = [publicacion1_1, publicacion1_3,publicacion2_2, publicacion3_1]
redD = (usuariosD, relacionesD, publicacionesD)
--
publicacionesE = [publicacion1_3, publicacion2_1, publicacion3_2, publicacion3_3]
redE = (usuariosD, relacionesD, publicacionesE)
--
publicacionesF = [publicacion1_1, publicacion1_3, publicacion3_1, publicacion4_2]
redF = (usuariosD, relacionesD, publicacionesF)
--
publicacionesG = [publicacion1_1, publicacion2_2, publicacion4_3]
redG = (usuariosD, relacionesD, publicacionesG)



