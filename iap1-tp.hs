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
nombresDeUsuarios (us,rs,ps) = proyectarNombres (usuarios(us,rs,ps))

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = eliminarRepetidos (nombreDeUsuario x : proyectarNombres xs)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos y (x:xs) | y == x = quitarTodos y xs
                     | otherwise = x : quitarTodos y xs
                     
eliminarRepetidos :: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x: eliminarRepetidos (quitarTodos x xs)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = relacionadosCon (relaciones(us,rs,ps)) u

relacionadosCon:: [Relacion] -> Usuario -> [Usuario]
relacionadosCon [] _ = []
relacionadosCon (x:xs) u | u == fst x = [snd x] ++ relacionadosCon xs u
                         | u == snd x = [fst x] ++ relacionadosCon xs u
                         | otherwise = relacionadosCon xs u

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = longitud (amigosDe (us,rs,ps) u) 

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = undefined

elAmigos :: RedSocial -> [Usuario] -> Usuario
--Requiere que la lista de usuarios no sea vacía
elAmigos (us,rs,ps) [x] = x
elAmigos (us,rs,ps) (x:y:xs) | (cantidadDeAmigos (us,rs,ps) x) >= (cantidadDeAmigos (us,rs,ps) y) = x
                             | otherwise = elAmigos (us,rs,ps) (y:xs)


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

--Funciones auxiliares (Predicados)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs) = y==x || pertenece y xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos (x:xs) (y:ys) = pertenece x (y:ys) && mismosElementos xs ys && pertenece y (x:xs) && mismosElementos ys xs

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
