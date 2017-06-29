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

let rec verif_pedido catalogo pedido = 
	match pedido with
    | [] -> true
    | (id, cant) :: xs -> if scd List.find (fun s -> fst s = id) catalogo >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						verif_pedido catalogo xs else false

let rec devolver_disponibles catalogo pedido = 
	match pedido with
	| [] -> []
	| (id, cant) :: xs -> (id, scd List.find (fun s -> fst s = id) catalogo) :: devolver_disponibles catalogo xs

let rec sumar_en_carrito carrito pedido = 
	match carrito with
	| [] -> 
		match pedido with
		| [] -> []
		| y :: ys -> y :: sumar_en_carrito [] ys

	| (id,cant) :: xs -> 
		match pedido with
		| [] -> (id, cant) :: modif_catalogo xs pedido
		| (id2, cant2) :: ys -> if id = id2 then (id, cant - cant2) :: modif_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else if id < id2 then (id, cant2) :: modif_catalogo xs pedido
			else (id, cant2) :: modif_catalogo carrito ys

let rec restar_en_catalogo catalogo pedido = 
	match catalogo with
	| [] -> []
	| (id,cant) :: xs -> 
		match pedido with
		| [] -> (id, cant) :: modif_catalogo xs pedido
		| (id2, cant2) :: ys -> if id = id2 then (id, cant - cant2) :: modif_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				(id, cant2) :: modif_catalogo xs pedido

let rec anular_carrito catalogo carrito = 
	match catalogo with
	| [] -> []
	| (id,cant) :: xs -> 
		match carrito with
		| [] -> (id, cant) :: modif_catalogo xs carrito
		| (id2, cant2) :: ys -> if id = id2 then (id, cant + cant2) :: modif_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				(id, cant2) :: modif_catalogo xs carrito



(* Intento aplicar el truco de pasar el catalogo y carrito sin q el cliente lo pueda usar. dsp reviso *)
let server s =	(* SIEMPRE LE PASO AL SERVER 1ro catalogo luego carrito luego lo q sea, y devuelvo de la misma forma *)
 	match S.branch s with
  	(* Recibimos catalogo, carrito y pedido. Verificamos que las cantidades del pedido puedan satisfacerse, agregamos esas cantidades al carrito y 
  	las quitamos del catalogo. Sino, devolvemos la lista de los elementos del pedido con las cant del catalogo *)
  	| `Pedir s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let p, s = S.receive s in
	       let r = verif_pedido n p in
	       if r = false then
		       let s = S.send n m (devolver_disponibles n p) in (* devuelvo el catalogo, carrito y la respuesta al pedido erroneo *)
		       (* enviar s a client() *)
		       S.close s
		   else 
		   		let s = S.send (restar_en_catalogo n p) (sumar_en_carrito m p) p in (* devuelvo el catalogo modificado y el carrito modificado (y p xq tal vez necesito recibir 3 cosas) *)
		       (* enviar s a client() *)
		       S.close s
	
	(* Si puedo, solicitar funciona en el lado cliente *)

	(*la idea es agarrar la lista del carrito y concatenarla a la lista del negocio  *)
  	| `Abandonar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (anular_carrito n m) [] in 	(* devuelvo el catalogo completo y el carrito bacio*)
	       S.close s
(*
	(* aca la idea es fijarse si el producto y su cantidad estan en el carrito. El elemento puede quedar con cant 0 *)
	| `Quitar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let q, s = S.receive s in

	(*aca hay que verificar que el carrito no se lleve mas plata de la que tiene en productos, para esto hay que recorrer la lista y sumar los precios * cantidad *)
	| `Finalizar s ->
*)

(* aca simulamos el cliente, y hacemos q llame al servidor con los distintos datos "hardcodeados". Como si el cliente lo recibiera de la persona. *)
let client =
  let s = Session.select (fun x -> `Add x) ep in (* select `Add operation *)
  let s = Session.send x s in
  let s = Session.send y s in
  let result, s = Session.receive s in
  Session.close s;
  result


let _  =
(* llamar a cliente*)
  let carrito = [] in
  let productos = [1;2;3] in
  let cantidades = [1;2;3] in
  let precios = [1;2;3] in
  	4 :: precios
