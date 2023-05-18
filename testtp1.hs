import Solucion (usuarios, publicaciones, relaciones, tieneUnSeguidorFiel)

--OBSERVACION EN GENERAL: TENER EN CUENTA QUE LAS REDES VAN CAMBIANDO EN LOS CASOS, AUNQUE A VECES PUEDEN REPETIRSE!

--Casos para la funcion ¨publicacionesDe¨
usuarios = [(1,"jorge"),(2,"mariela"),(3,"juan")]
relaciones = [((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))]
publicaciones = [((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]
--para este test la red se limitará a estas publicaciones
--publicacion2_1: ((2,"mariela"),"hi",[])
--publicacion2_2: ((2,"mariela"),"bye",[(1,"jorge")])
--publicacion3_1: ((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])

--Para los 3 casos se utilizará una misma RedSocial
RedSocial: ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) 

--1--Caso en que el usuario no haya realizado ninguna publicacion:
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (1,"jorge")
usuario1 = (1,"jorge")
devuelve = []

--2--Caso en que el usuario tenga exactamente una publicacion:
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (3,"juan")
usuario3 = (3,"juan")
devuelve = [((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]

--3--Caso en que el usuario tenga mas de una publicacion:
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (2,"mariela") 
usuario2 = (2,"mariela")
devuelve = [((2,"mariela"),"hi",[]),((2,"mariela"),"bye",[(1,"jorge")])]

---




--Casos para la funcion ´publicacionesQueLeGustanA´
usuarios = [(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")]
relaciones = [((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))]
publicaciones= [((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])] 
--publicacion4_1: ((4,"eduardo"),"Me aburro",[(8,"marcos")])
--publicacion5_1: ((5,"federico"),"Hola a todos",[])
--publicacion5_2: ((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")])
--publicacion6_1: ((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")])
--publicacion6_2: ((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")])
--publicacion7_1: ((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]) 
--publicacion7_2: ((7,"kira"),"Chau",[(8,"marcos")])

--Para los 5 casos se utilizará una misma RedSocial
RedSocial = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])])

--1--Caso en el que el usuario no likee ninguna publicacion
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]) (7,"kira") 
usuario7 = (7,"kira")
devuelve = []

--2--Caso en el que el usuario likee exactamente una sola publicacion
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]) (4,"eduardo") 
usuario4 = (4,"eduardo")
devuelve = [((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")])]

--3-- Caso en el que el usuario likee más de una publicacion de un mismo usuario
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]) (5,"federico") 
usuario5 = (5,"federico")
devuelve = [((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")])]

--4-- Caso en el que el usuario likee exactamente una publicacion de dos usuarios distintos
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]) (6,"julian") 
usuario6 = (6,"julian")
devuelve = [((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")])]

--5-- Caso en el que el usuario likee varias publicaciones de distintos usuarios
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((4,"eduardo"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Me aburro",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]) (8,"marcos") 
usuario8 = (8,"marcos")
devuelve = [((4,"eduardo"),"Me aburro",[(8,"marcos")]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((7,"kira"),"Chau",[(8,"marcos")])]




-- Casos para la funcion ´lesGustanLasMismasPublicaciones´ 
usuarios = [(1,"jorge"),(2,"mariela"),(3,"juan")]
relaciones = [((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))]
--publicaciones = 
--para este test se tomaran las siguientes publicaciones (OJO QUE PARA CADA CASO LAS REDES VAN CAMBIANDO): 
--publicacion1_1: ((1,"jorge"),"Moon",[(3,"juan")])
--publicacion2_1: ((2,"mariela"),"hi",[])-*-
--publicacion2_2: ((2,"mariela"),"bye",[(1,"jorge")])-*-
--publicacion2_3: ((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")])
--publicacion3_1: ((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])-*-
--publicacion3_2: ((3,"juan"),"Sun",[(3,"juan")])

--los * son para no perder de vista las publicaciones que se utilizaron antes (para la redsocial del test del ej6) 

--1--Caso en el que el usuario1 likee exactamente las mismas publicaciones que el usuario2: True
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((2,"mariela"),"hi",[]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (1,"jorge") (2,"mariela")
publicaciones de la redSocial = [((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((2,"mariela"),"hi",[]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]
usuario1 = (1,"jorge")
usuario2 = (2,"mariela")
devuelve = True
    -- Jorge = Mariela[((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]

--2--Caso en el que usuario1 y el usuario2 likeen publicaciones distintas pero con alguna en comun: False
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"bye",[(1,"jorge")]),((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (1,"jorge") (2,"mariela")
publicaciones de la redSocial = [((2,"mariela"),"bye",[(1,"jorge")]),((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]
usuario1 = (1,"jorge")
usuario2 = (2,"mariela")
devuelve = False
    -- Jorge = [((2,"mariela"),"bye",[(1,"jorge")]),((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]
    -- Mariela = [((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]


--3--Caso en el que el usuario1 y el usuario2 likeen publicaciones distintas sin tener ninguna en comun: False
entrada = ([(1,"jorge"),(2,"mariela"),(3,"juan")],[((1,"jorge"),(2,"mariela")),((1,"jorge"),(3,"juan")),((2,"mariela"),(3,"juan"))],[((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((1,"jorge"),"Moon",[(3,"juan")]),((2,"mariela"),"hi",[]),((3,"juan"),"Sun",[(3,"juan")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]) (2,"mariela") (3,"juan")
publicaciones de la redSocial = [((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")]),((1,"jorge"),"Moon",[(3,"juan")]),((2,"mariela"),"hi",[]),((3,"juan"),"Sun",[(3,"juan")]),((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")])]
usuario1 = (2,"mariela")
usuario2 = (3,"juan")
devuelve = False
   -- Juan = [((1,"jorge"),"Moon",[(3,"juan")]),((3,"juan"),"Sun",[(3,"juan")])]
   -- Mariela = [((3,"juan"),"hey",[(2,"mariela"),(1,"jorge")]),((2,"mariela"),"Good Morning",[(2,"mariela"),(1,"jorge")])]


 
 
 --Caso para la funcion ´tieneUnSeguidorFiel´
usuarios = [(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")]
relaciones de la red social = [((4,"eduardo"),(5,"federico")),((8,"marcos"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))]
--es una relacion de redsocial DISTINTA a la del ejercicio 7.
--publicaciones = 
--publicacion4_1: ((4,"eduardo"),"Me aburro",[(8,"marcos")]) *
--publicacion4_2: ((4,"eduardo"),"Feliz",[(8,"marcos")])
--publicacion5_1: ((5,"federico"),"Hola a todos",[]) *
--publicacion5_2: ((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]) *
--publicacion6_1: ((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]) *
--publicacion6_2: ((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")]) *
--publicacion7_1: ((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")])  *
--publicacion7_2: ((7,"kira"),"Chau",[(8,"marcos")]) *

 --1--Caso con un solo seguidor fiel: True                                                                           en vez de edu marcos
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((8,"marcos"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((4,"eduardo"),"Feliz",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((4,"eduardo"),"Me aburro",[(8,"marcos")])]) (4,"eduardo")
publicaciones de la red social = [((4,"eduardo"),"Feliz",[(8,"marcos")]),((5,"federico"),"Hola a todos",[]),((4,"eduardo"),"Me aburro",[(8,"marcos")])]
usuario4 = (4,"eduardo")
seguidorfiel = (8,"marcos")

 --2--Caso con mas de un seguidor fiel: True
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((8,"marcos"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")])]) (6,"julian")
publicaciones de la red social = [((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")]),((7,"kira"),"No molestar",[(6,"julian"),(8,"marcos")]),((6,"julian"),"Laburando",[(5,"federico"),(8,"marcos")])] 
usuario6 = (6,"julian") 
seguidorfiel = (8,"marcos") y (5,"federico")

 --3--Caso con ningun seguidor fiel: False 
entrada = ([(4,"eduardo"),(5,"federico"),(6,"julian"),(7,"kira"),(8,"marcos")],[((4,"eduardo"),(5,"federico")),((8,"marcos"),(6,"julian")),((5,"federico"),(6,"julian")),((7,"kira"),(5,"federico")),((8,"marcos"),(4,"eduardo"))],[((5,"federico"),"Hola a todos",[]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")])]) (5,"federico")
publicaciones de la red social = [((5,"federico"),"Hola a todos",[]),((6,"julian"),"Buen Dia",[(5,"federico"),(8,"marcos")]),((5,"federico"),"Buenas Tardes",[(4,"eduardo"),(6,"julian")])]
usuario5 = (5,"federico")
seguidorfiel = []    