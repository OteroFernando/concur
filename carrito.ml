(* IDEAS SUELTAS: [5; 4; 3; 2; 1] esto es una lista las tuplas se declaran = ("larry", 47, 165, ’M’) el tp lo podemos hacer con tres listas(producto,precio,cantidad) para evitar la tupla, pero como quieras.. no se hacer pi1, la recursion en las listas es como haskell  1 :: (2 :: (3 :: (4 :: (5 :: [])))) aparentemente existen funciones de lista del estilo List.sort , dejo algunos links interesantes: https://cseweb.ucsd.edu/classes/wi11/cse130/discussion/ocaml-intro2.pdf aca hay links sobre listas y tuplas en ocaml. Aparentemente no se pueden declarar variables globales y si se puede no se como hacerlo, sugiero que desde el main creemos las dos listas o la lista de tuplas (yo prefiero dos listas). el let se corre una vez, el let rec es recursivo.    *)


module Session = Session.Bare

let abandonar carrito productos cantidades precios =
  let rec aux l c =(*la idea es agarrar la lista del carrito y concatenarla a la lista del negocio  *)


let pedir carrito productos cantidades precios = (*aca el tipo recibe el producto y la cantidad y nos tenemos que fijar si esas cantidades estan en los productos y en cantidades   *)


let quitar producto cantidad carrito productos cantidades precios = (* aca la idea es fijarse si el producto y su cantidad estan en el carrito y si al quitarlos la cantidad de del producto en el carrito es cero, se debe quitar del carrito *)

let finalizar carrito = (*aca hay que verificar que el carrito no se lleve mas plata de la que tiene en productos, para esto hay que recorrer la lista y sumar los precios * cantidad *)

let solicitar = (* aca hay que retornar el contenido del carrito y sumar el monto total de la compra *)


