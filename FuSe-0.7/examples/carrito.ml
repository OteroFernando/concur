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

module S = Session.Bare

(* auxiliares de pedido *)
let rec verif_pedido catalogo pedido = 
	match pedido with
    | [] -> true
    | (id, cant) :: xs -> let (a,b) = List.find (fun s -> fst s = id) catalogo in
    						if b  >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						verif_pedido catalogo xs else false

let rec devolver_disponibles catalogo pedido = 
	match pedido with
	| [] -> []
	| (id, cant) :: xs -> let (a,b) = List.find (fun s -> fst s = id) catalogo in
							(id, b) :: devolver_disponibles catalogo xs			

let rec sumar_en_carrito carrito pedido = 
	match carrito with
	| [] -> 
		match pedido with
		| [] -> []
		| y :: ys -> y :: sumar_en_carrito [] ys

	| (id,cant) :: xs -> 
		match pedido with
		| [] -> (id, cant) :: sumar_en_carrito xs []
		| (id2, cant2) :: ys -> if id = id2 then (id, cant + cant2) :: sumar_en_carrito xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else if id < id2 then (id, cant2) :: sumar_en_carrito xs pedido
			else (id, cant2) :: sumar_en_carrito carrito ys

let rec restar_en_catalogo catalogo pedido = 
	match catalogo with
	| [] -> []
	| (id,cant) :: xs -> 
		match pedido with
		| [] -> (id, cant) :: restar_en_catalogo xs pedido
		| (id2, cant2) :: ys -> if id = id2 then (id, cant - cant2) :: restar_en_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				(id, cant2) :: restar_en_catalogo xs pedido

(* auxiliares de avandonar*)
let rec anular_carrito catalogo carrito = 
	match catalogo with
	| [] -> []
	| (id,cant) :: xs -> 
		match carrito with
		| [] -> (id, cant) :: anular_carrito xs carrito
		| (id2, cant2) :: ys -> if id = id2 then (id, cant + cant2) :: anular_carrito xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				(id, cant2) :: anular_carrito xs carrito

(* auxiliares de quitar *)
let rec quitar_carrito carrito producto = 
	match carrito with
	| [] -> []
	| (id, cant) :: xs -> let (a,b) = producto in
							if a = id then 
								if b < cant then (id, cant - b) :: quitar_carrito xs producto
								else quitar_carrito xs producto
							else (id, cant) :: quitar_carrito xs producto

let rec diferencia carrito producto = 
	match carrito with
	| [] -> 0
	| (id, cant) :: xs -> let (a,b) = producto in
							if a = id then 
								if b < cant then b
								else cant
							else diferencia xs producto

let rec devolver_a_catalogo catalogo carrito producto = 
	match catalogo with
	| [] -> []
	| (id, cant) :: xs -> let (a,b) = producto in
							if a = id then 
								let d = diferencia carrito producto in
								(id, cant + d) :: devolver_a_catalogo xs carrito producto
							else
								(id, cant) :: devolver_a_catalogo xs carrito producto

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
		       let s = S.send n m (devolver_disponibles n p) s in (* devuelvo el catalogo, carrito y la respuesta al pedido erroneo *)
		       (* enviar s a client() *)
		       S.close s
		   else 
		   		let s = S.send (restar_en_catalogo n p) (sumar_en_carrito m p) p s in (* devuelvo el catalogo modificado y el carrito modificado (y p xq tal vez necesito recibir 3 cosas) *)
		       (* enviar s a client() *)
		       S.close s
	
	(* Si puedo, solicitar funciona en el lado cliente *)

	(*la idea es agarrar la lista del carrito y concatenarla a la lista del negocio  *)
  	| `Abandonar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (anular_carrito n m) [] in 	(* devuelvo el catalogo completo y el carrito bacio*)
	       S.close s

	(* quita el elemento pedido del carrito. Si se quieren quitar mas o los mismos elementos que hay, se quita el item *)
	| `Quitar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let q, s = S.receive s in
	       let s = S.send (devolver_a_catalogo n m q) (quitar_carrito m q) s in
	       (* enviar s a client() *)
		    S.close s
(*
	(*aca hay que verificar que el carrito no se lleve mas plata de la que tiene en productos, para esto hay que recorrer la lista y sumar los precios * cantidad *)
	| `Finalizar s ->
*)

(* aca simulamos el cliente, y hacemos q llame al servidor con los distintos datos "hardcodeados". Como si el cliente lo recibiera de la persona. *)
let client s =
  let s = S.select (fun x -> `Pedir x) ep in (* select `Add operation *)
  let s = S.send x s in
  let s = S.send y s in
  let result, s = S.receive s in
  S.close s;
  result


let _ =
  let a, b = S.create () in
  let _ = Thread.create client a in
  print_int (math_client b);
  print_newline()
