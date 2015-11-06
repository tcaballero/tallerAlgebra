data RT t = Rose t [RT t] deriving Show

r1 = Rose 20 []
r2 = Rose 20 [Rose 33 [], Rose 17 [], Rose 34 [], Rose 33 []]
r3 = Rose 15 [Rose 16 [Rose 17 [], Rose 18 []], Rose 8 []]
r4 = Rose '9' [Rose 'a' [Rose 'b' [Rose 'c' []], Rose 'l' [], Rose 'b' []], Rose '9' []]
r7 = Rose '1' [Rose 'a' [Rose 'b' [r4, r4]]]

raiz :: RT t -> t
raiz (Rose raiz _) = raiz

hijos :: RT t -> [RT t]
hijos (Rose _ nodos) = nodos

sumarTodos :: Num t => RT t -> t
sumarTodos (Rose nodo []) = nodo
sumarTodos (Rose nodo ramas) = nodo + 

nodosLista :: Num t => [RT t] -> [t]
nodosLista [] = []
nodosLista (x:xs) = [raiz x] ++ (nodosLista (hijos x)) ++ nodosLista xs