(* IDEAS SUELTAS: 
	[5; 4; 3; 2; 1] esto es una lista 
	las tuplas se declaran = ("larry", 47, 165, ’M’) 
	el tp lo podemos hacer con tres listas(producto,precio,cantidad) para evitar la tupla, pero como quieras.. 
	no se hacer pi1, la recursion en las listas es como haskell  1 :: (2 :: (3 :: (4 :: (5 :: [])))) 
	aparentemente existen funciones de lista del estilo List.sort , 
	dejo algunos links interesantes: https://cseweb.ucsd.edu/classes/wi11/cse130/discussion/ocaml-intro2.pdf aca hay links sobre listas y tuplas en ocaml. 
	Aparentemente no se pueden declarar variables globales y si se puede no se como hacerlo, sugiero que desde el main creemos las dos listas o la lista 
	de tuplas (yo prefiero dos listas). 
	el let se corre una vez, el let rec es recursivo.    *)

(*type pair_of_ints = { a : int; b : int } *)

module Session = Session.Bare

let rec verif_pedido catalogo (carrito, valido) =	(* armo el carrito (valido = true) o la lista a devolver (valido = false) *)
	match carrito with
    | [] -> ([], valido)
    | (id, cant) :: xs -> if sec List.find (fun s -> fst s = id) catalogo >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						((id, cant) :: f_solicitar catalogo xs, valido) else ((id, sec s) :: f_solicitar catalogo xs, false)

(*La idea es q se le pueda pasar al server una tira de instrucciones*)

(*de momento esta funcion no esta hecha D: *)

let rec server s =
  match S.branch s with
    `Quit s -> S.close s
  | `Solicitar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       f_solicitar n m
  | `Mult s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (n * m) s in
	       server s
  | `Div s ->  let n, s = S.receive s in
               let m, s = S.receive s in
               let s = S.send (n / m) s in
               let s = S.send (n mod m) s in
               server s
  | `Neg s  -> let n, s = S.receive s in
	       let s = S.send (-n) s in
	       server s
  | `Eq s   -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (n = m) s in
	       server s
(*
let abandonar carrito productos cantidades precios =
  let rec aux l c =(*la idea es agarrar la lista del carrito y concatenarla a la lista del negocio  *)


let pedir carrito productos cantidades precios = (*aca el tipo recibe el producto y la cantidad y nos tenemos que fijar si esas cantidades estan en los productos y en cantidades   *)


let quitar producto cantidad carrito productos cantidades precios = (* aca la idea es fijarse si el producto y su cantidad estan en el carrito y si al quitarlos la cantidad de del producto en el carrito es cero, se debe quitar del carrito *)

let finalizar carrito = (*aca hay que verificar que el carrito no se lleve mas plata de la que tiene en productos, para esto hay que recorrer la lista y sumar los precios * cantidad *)

let solicitar = (* aca hay que retornar el contenido del carrito y sumar el monto total de la compra *)
*)

let _  =
  let carrito = [] in
  let productos = [1;2;3] in
  let cantidades = [1;2;3] in
  let precios = [1;2;3] in
