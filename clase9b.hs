-- ARBOLES --
data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t) deriving Show

esHoja :: Arbol a -> Bool
esHoja (Hoja _) = True
esHoja _ = False

maximo :: Ord a => Arbol a -> a
maximo (Hoja h) = h
maximo (Ramif aIzq nodo aDer) = max nodo (max (maximo aIzq) (maximo aDer))

raiz :: Arbol a -> a
raiz (Hoja h) = h
raiz (Ramif aIzq nodo aDer) = nodo

todosIguales :: Eq a => Arbol a -> Bool
todosIguales (Hoja h) = True
todosIguales (Ramif aIzq nodo aDer) = (nodo == (raiz aDer)) && (nodo == (raiz aIzq)) && (todosIguales aIzq) && (todosIguales aDer)

espejar :: Arbol a -> Arbol a
espejar (Hoja h) = Hoja h
espejar (Ramif aIzq nodo aDer) = Ramif (espejar aDer) nodo (espejar aIzq)

esHeap :: Ord a => Arbol a -> Bool
esHeap (Ramif (Hoja hijo1) padre (Hoja hijo2)) = (padre >= hijo1) && (padre >= hijo2)
esHeap (Ramif aIzq nodo aDer) = (esHeap aIzq) && (esHeap aDer)

-- LISTAS --
data Lista a = Vacia | Agregar a (Lista a) deriving Show

vacia :: Lista a -> Bool
vacia Vacia = True
vacia _ = False

suma :: Lista Float -> Float
suma Vacia = 0
suma (Agregar item lista) = item + suma lista

enPosicion :: Lista a -> Integer -> a
enPosicion (Agregar item lista) index
        | index == 1 = item
        | otherwise = enPosicion lista (index-1)
