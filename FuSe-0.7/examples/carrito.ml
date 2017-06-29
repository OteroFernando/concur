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

(*
	Tipos:
	catalogo es una lista de 3-upla id,cant,precio
	carrito y pedido son listas de 2-uplas id,cant
	producto es una 2-upla id,cant
 *)

module S = Session.Bare

let rec find2 lista elem =
	match lista with
	| [] -> (-1,-1,-1)
	| (a,b,c) :: xs -> if a = elem then (a,b,c) else find2 xs elem

(* auxiliares de pedido *)
let rec verif_pedido catalogo pedido = 
	match pedido with
    | [] -> true
    | (id, cant) :: xs -> let (a,b,c) = find2 catalogo id in
    						if b  >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						verif_pedido catalogo xs else false

let rec devolver_disponibles catalogo pedido = 
	match pedido with
	| [] -> []
	| (id, cant) :: xs -> let (a,b,c) = find2 catalogo id in
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
	| (id, cant, precio) :: xs -> 
		match pedido with
		| [] -> (id, cant) :: restar_en_catalogo xs pedido
		| (id2, cant2) :: ys -> if id = id2 then (id, cant - cant2) :: restar_en_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				(id, cant2) :: restar_en_catalogo xs pedido
(* auxiliar de solicitar *)
let rec solicitar catalogo carrito = 
	match carrito with
	| [] -> 0
	| (id, cant) :: xs -> let (a,b,c) = find2 catalogo id in
							cant*c + solicitar catalogo xs	

(* auxiliar de avandonar*)
let rec anular_carrito catalogo carrito = 
	match catalogo with
	| [] -> []
	| (id, cant, precio) :: xs -> 
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
	| (id, cant, precio) :: xs -> let (a,b) = producto in
							if a = id then 
								let d = diferencia carrito producto in
								(id, cant + d) :: devolver_a_catalogo xs carrito producto
							else
								(id, cant) :: devolver_a_catalogo xs carrito producto
(* auxiliares de finalizar *)
let rec cobrar catalogo carrito = 
	match carrito with
    | [] -> true
    | (id, cant) :: xs -> let (a,b,c) = find2 catalogo id in
    						if b  >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						cobrar catalogo xs else false



(* Intento aplicar el truco de pasar el catalogo y carrito sin q el cliente lo pueda usar. dsp reviso *)
let rec server s =	(* SIEMPRE LE PASO AL SERVER 1ro catalogo 2do carrito luego lo q sea, y devuelvo de la misma forma *)
 	match S.branch s with
  	(* Recibimos catalogo, carrito y pedido. Verificamos que las cantidades del pedido puedan satisfacerse, agregamos esas cantidades al carrito y 
  	las quitamos del catalogo. Sino, devolvemos la lista de los elementos del pedido con las cant del catalogo *)
  	| `Pedir s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let p, s = S.receive s in
	       let r = verif_pedido n p in
	       if r = false then
		      (* let s = S.send n s in 
		       let s = S.send m s in *)
		       let s = S.send (devolver_disponibles n p) s in (* devuelvo el catalogo, carrito y la respuesta al pedido erroneo *)
		       (* enviar s a client() *)
		       server s
		   else 
		   		let s = S.send (restar_en_catalogo n p) s in (* devuelvo el catalogo modificado y el carrito modificado *)
		       (*	let s = S.send (sumar_en_carrito m p) s in *)
		       (* enviar s a client() *)
		       server s
	
	(* Solicitar multiplica cant * precio para cada elemento del carrito *)
	| `Solicitar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send m s in 	(* este tal vez no es necesario *)
	       let s = S.send (solicitar n m) s in 	(* devuelvo el catalogo completo y el carrito bacio*)
	       server s

	(*la idea es agarrar la lista del carrito y volcarla en el catalogo  *)
  	| `Abandonar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (anular_carrito n m) s in 	(* devuelvo el catalogo completo. Y el carrito vacio?*)
	       S.close s

	(* quita el elemento pedido del carrito. Si se quieren quitar mas o los mismos elementos que hay, se quita el item *)
	| `Quitar s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let q, s = S.receive s in
	       let s = S.send (devolver_a_catalogo n m q) s in
	       let s = S.send (quitar_carrito m q) s in
	       (* enviar s a client() *)
		    server s

	(*aca hay que verificar que el carrito no se lleve mas plata de la que tiene en productos, para esto hay que recorrer la lista y sumar los precios * cantidad *)
	| `Finalizar1 s -> let n, s = S.receive s in
	       let m, s = S.receive s in
               let r = Random.bool () in
               if r = false then
	       	    let s = S.send (solicitar n m) s in 	(* falta q si falla la transaccion, no cierre el vinculo *)
	      	    S.close s
   	       else
			(* fallo la transaccion, reintentar? *)
		    server s

(* aca simulamos el cliente, y hacemos q llame al servidor con los distintos datos "hardcodeados". Como si el cliente lo recibiera de la persona. *)
let client s x y =
  let s = S.select (fun x -> `Pedir x) s in (* select `Add operation *)
  let s = S.send x s in
  let s = S.send y s in
  let result, s = S.receive s in
  S.close s;
  result


let _ =
  let a, b = S.create () in
  let _ = Thread.create client a in
 (* print_int (math_client b); *)
  print_newline()
