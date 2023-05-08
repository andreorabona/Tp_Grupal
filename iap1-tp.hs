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

--Ejercicio 1

-- describir qué hace la función: Lo que hace esta función es agarrar la lista de los usuarios de una RedSocial y darnos una lista con todos los nombres de los usuarios (y sin repetirse)
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = proyectarNombres (usuarios(us,rs,ps))

-- En esta función se nos da una lista de usuarios y lo que hacemos es "extraer" solo los nombres para ponerlos en otra lista
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = eliminarRepetidos (nombreDeUsuario x : proyectarNombres xs)

-- Las siguientes funciones son auxiliares que nos sirven para quitar los casos repetidos de nombres
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos y (x:xs) | y == x = quitarTodos y xs
                     | otherwise = x : quitarTodos y xs
                     
eliminarRepetidos :: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x: eliminarRepetidos (quitarTodos x xs)

--Ejercicio 2

-- describir qué hace la función: Esta función nos sirve para saber cuales son los amigos de el usuario que le consultemos, tenemos una RedSocial y le damos un usuario, entonces esta función nos da la lista de usuarios que son sus amigos (que están relacionados con el)
-- Observación: Como se requiere una RedSocialValida entonces con esta función aseguramos que no van a haber repetidos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = relacionadosCon (relaciones(us,rs,ps)) u

{- Con esta función nosotros queremos ver quienes están relacionados con el usuario que le pidamos.
Lo que hace la función es que dada una lista de relaciones y un usuario la función mira la primera relación de la lista y si el usuario es el primero de la relación nos da el segundo y luego usamos la recursión para ver las demás relaciones (de forma analoga si el usuario es el segundo)
Además los ponemos en forma de lista para así luego concatenarlos y obtener la lista de usuarios relacionados con el usuario pedido-}
relacionadosCon:: [Relacion] -> Usuario -> [Usuario]
relacionadosCon [] _ = []
relacionadosCon (x:xs) u | u == fst x = [snd x] ++ relacionadosCon xs u
                         | u == snd x = [fst x] ++ relacionadosCon xs u
                         | otherwise = relacionadosCon xs u

--Ejercicio 3

-- describir qué hace la función: Esta función lo que hace es darnos la cantidad de amigos de un usuario.
-- Lo que hace es calcular la longitud de la lista de amigos del usuario pedido usando la función del ejercicio anterior amigosDe
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = longitud (amigosDe (us,rs,ps) u) 

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Ejercicio 4

-- describir qué hace la función: Esta función nos permite saber quien es el usuario que tiene más amigos dada una RedSocial
-- Utilizando una función auxiliar podemos saber quien es el usuario con más amigos en la lista de usuarios de la RedSocial
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = elAmigos (us,rs,ps) (usuarios (us,rs,ps))

-- Este auxiliar lo que hace es que dado una RedSocial y una lista de usuarios entonces compara la cantidad de amigos del primer usuario con el segundo usuario de la lista quedandose con el usuario de mayor cantidad de amigos y luego lo sigue comparando con el resto de la lista
elAmigos :: RedSocial -> [Usuario] -> Usuario
--Requiere que la lista de usuarios no sea vacía
elAmigos (us,rs,ps) [x] = x
elAmigos (us,rs,ps) (x:y:xs) | (cantidadDeAmigos (us,rs,ps) x) >= (cantidadDeAmigos (us,rs,ps) y) = elAmigos (us,rs,ps) (x:xs)
                             | otherwise = elAmigos (us,rs,ps) (y:xs)

--Ejercicio 5

-- describir qué hace la función: Esta función nos dice si hay o no un usuario que tenga más de un millón de amigos (como Roberto Carlos)
-- Las dos funciones auxiliares nos sirven para verificar si en una lista de usuarios de una RedSocial existe algún usuario que tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,rs,ps) = existeMillonDeAmigos (us,rs,ps) (usuarios (us,rs,ps))

-- Auxiliares
millonDeAmigos :: RedSocial -> Usuario -> Bool
millonDeAmigos (us,rs,ps) u = cantidadDeAmigos (us,rs,ps) u > 1000000

existeMillonDeAmigos :: RedSocial -> [Usuario] -> Bool
existeMillonDeAmigos (us,rs,ps) [] = False
existeMillonDeAmigos (us,rs,ps) (x:xs) = millonDeAmigos (us,rs,ps) x || existeMillonDeAmigos (us,rs,ps) xs

--Ejercicio 6

-- describir qué hace la función: Esta función lo que hace es mostrarnos las publicaciones que "hizo" el usuario pedido
-- Con una función auxiliar lo que hacemos es recorrer la lista de publicaciones y si el usuario de la publibación coincide con el usuario que queremos saber entonces esa publicación en una lista.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) u = proyectarPublicacionesDe (publicaciones(us,rs,ps)) u

-- Auxiliar

proyectarPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
proyectarPublicacionesDe [] _ = []
proyectarPublicacionesDe (x:xs) u | (usuarioDePublicacion x == u) = x : proyectarPublicacionesDe xs u
                                  | otherwise = proyectarPublicacionesDe xs u

--Ejercicio 7

-- describir qué hace la función: Esta función lo que hace es darnos una lista de las publicaciones a las que el usuario pedido les dio "like"
-- Con una función auxiliar lo que hacemos es recorrer la lista de publicaciones y si el usuario que queremos saber pertenece a la lista de usuarios que le dio me gusta en la publicacion entonces esa publicación la ponemos en una lista
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,ps) u = proyectarPublicacionesQueLeGustanA (publicaciones (us,rs,ps)) u

-- Auxiliar

proyectarPublicacionesQueLeGustanA :: [Publicacion] -> Usuario -> [Publicacion]
proyectarPublicacionesQueLeGustanA [] _ = []
proyectarPublicacionesQueLeGustanA (x:xs) u | pertenece u (likesDePublicacion x) = x : proyectarPublicacionesQueLeGustanA xs u
                                            | otherwise = proyectarPublicacionesQueLeGustanA xs u

--Ejercicio 8

-- describir qué hace la función: Esta función nos da verdadero o falso dependiendo de si las publicaciones que le gustan al u1 son iguales que las que le gustan al u2
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones (us,rs,ps) u1 u2 = publicacionesQueLeGustanA (us,rs,ps) u1 == publicacionesQueLeGustanA (us,rs,ps) u2

--Ejercicio 9

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

--Ejercicio 10

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

--Funciones auxiliares (Predicados)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs) = y==x || pertenece y xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = estaIncluido xs ys && estaIncluido ys xs

-- Auxiliar
estaIncluido :: (Eq t) => [t] -> [t] -> Bool
estaIncluido [] _ = True
estaIncluido (x:xs) ys = pertenece x ys && estaIncluido xs ys

redSocialValida :: RedSocial -> Bool

redSocialValida (us,rs,ps) = usuariosValidos (usuarios(us,rs,ps)) && relacionesValidas (usuarios(us,rs,ps)) (relaciones(us,rs,ps)) && publicacionesValidas (usuarios(us,rs,ps)) (publicaciones(us,rs,ps))

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = False
usuariosValidos [x] = usuarioValido x
usuariosValidos (x:xs) | (usuarioValido x == True) && noHayIdsRepetidos (x:xs) = usuariosValidos xs
                       | otherwise = False

usuarioValido :: Usuario -> Bool
usuarioValido (_,"") = False
usuarioValido (id,_) = idDeUsuario (id,"nombre") > 0

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos [x] = True
noHayIdsRepetidos (x:y:xs) | (idDeUsuario x == idDeUsuario y) = False
                           | otherwise = noHayIdsRepetidos (x:xs) && noHayIdsRepetidos (y:xs)

relacionesValidas :: [Usuario] -> [Relacion] -> Bool

relacionesValidas xs ys = usuariosDeRelacionValidos xs ys && relacionesAsimetricas ys && noHayRelacionesRepetidas ys

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos [] _ = False
usuariosDeRelacionValidos xs (y:ys) = (fst y /= snd y) && pertenece (fst y) xs && pertenece (snd y) xs && usuariosDeRelacionValidos xs ys

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas [x] = True
relacionesAsimetricas (x:y:xs) | (fst x == snd y) && (fst y == snd x) = False
                               | otherwise = relacionesAsimetricas (x:xs) && relacionesAsimetricas (y:xs)

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas [x] = True
noHayRelacionesRepetidas (x:y:xs) | (idDeUsuario (fst x) == idDeUsuario (fst y)) && (idDeUsuario (snd x) == idDeUsuario (snd y)) = False
                                  | otherwise = noHayRelacionesRepetidas (x:xs) && noHayRelacionesRepetidas (y:xs)

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool

publicacionesValidas xs ys = usuariosDePublicacionSonUsuariosDeRed xs ys && usuariosDeLikeDePublicacionSonUsuariosDeRed xs ys && noHayPublicacionesRepetidas ys

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed [] _ = False
usuariosDePublicacionSonUsuariosDeRed xs (y:ys) | not(pertenece (usuarioDePublicacion y) xs) = False
                                                | otherwise = usuariosDePublicacionSonUsuariosDeRed xs ys

usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed _ [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed [] _ = False
usuariosDeLikeDePublicacionSonUsuariosDeRed xs (y:ys) = usuariosLikeValidos (likesDePublicacion y) xs && usuariosDeLikeDePublicacionSonUsuariosDeRed xs ys

usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos _ [] = True
usuariosLikeValidos [] _ = False
usuariosLikeValidos xs (y:ys) = pertenece y xs && usuariosLikeValidos xs ys

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas [x] = True
noHayPublicacionesRepetidas (x:y:xs) | x==y = False--(idDeUsuario (usuarioDePublicacion x) == idDeUsuario (usuarioDePublicacion y)) || x==y = False
                                     | otherwise = noHayPublicacionesRepetidas (x:xs) && noHayPublicacionesRepetidas (y:xs)

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] (us,rs,ps) = True
cadenaDeAmigos [x] (us,rs,ps) = True
cadenaDeAmigos (x:y:xs) (us,rs,ps) | (relacionadosDirecto x y (us,rs,ps) == False) = False
                                   | otherwise = cadenaDeAmigos (y:xs) (us,rs,ps)

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool

relacionadosDirecto (id1,nombre1) (id2,nombre2) (us,rs,ps) = (pertenece ((id1,nombre1),(id2,nombre2)) (relaciones (us,rs,ps))) || (pertenece ((id2,nombre2),(id1,nombre1)) (relaciones (us,rs,ps)))

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed (us,rs,ps) [] = True
sonDeLaRed (us,rs,ps) (x:xs) = pertenece x (usuarios(us,rs,ps)) && sonDeLaRed (us,rs,ps) xs

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e xs = e == head xs

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e xs = e == last xs

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos [x] = True
sinRepetidos (x:y:xs) = (x/=y) && sinRepetidos (y:xs) && sinRepetidos (x:xs)

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

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


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3]
relacionesC = []
publicacionesC = [publicacion1_1, publicacion1_2, publicacion2_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)