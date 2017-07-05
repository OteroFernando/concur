(*
	Tipos:
	catalogo es una lista de 3-upla  ((id,cant),precio)
	carrito y pedido son listas de 2-uplas id,cant
	producto es una 2-upla id,cant
 *)

module S = Session.Bare

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | (a,b)::t -> print_string "("; f a; print_string ","; f b; print_string ")"; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;

let rec aux_catalogo i n catalogo =
	Random.init(n);
	if i < n then
	let a = ((i, (Random.int n) +1), (Random.int n) +1) in
	a :: aux_catalogo (i+1) n catalogo
 	else []

let crear_catalogo n =
	aux_catalogo 0 n []

let rec find2 lista elem =
	match lista with
	| [] -> ((-1,-1),-1)
	| ((a,b),c) :: xs -> if a = elem then ((a,b),c) else find2 xs elem

(* auxiliares de pedido *)
let rec verif_pedido catalogo pedido =
	match pedido with
    | [] -> true
    | (id, cant) :: xs -> let ((_,b),_) = find2 catalogo id in
    						if b  >= cant then (* List.find (s -> cond) catalogo devuelve el elemento s en la lista catalogo si lo encuentra *)
    						verif_pedido catalogo xs else false

let rec devolver_disponibles catalogo pedido =
	match pedido with
	| [] -> []
	| (id, _) :: xs -> let ((_,b),_) = find2 catalogo id in
							(id, b) :: devolver_disponibles catalogo xs

let rec sumar_en_carrito carrito pedido =
	match carrito with
	| [] ->
		(match pedido with
		| [] -> []
		| y :: ys -> y :: sumar_en_carrito [] ys)

	| (id,cant) :: xs ->
		(match pedido with
		| [] -> (id, cant) :: sumar_en_carrito xs []
		| (id2, cant2) :: ys -> if id = id2 then (id, cant + cant2) :: sumar_en_carrito xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else if id < id2 then (id, cant2) :: sumar_en_carrito xs pedido
			else (id, cant2) :: sumar_en_carrito carrito ys)

let rec restar_en_catalogo catalogo pedido =
	match catalogo with
	| [] -> []
	| ((id, cant), precio) :: xs ->
		match pedido with
		| [] -> ((id, cant), precio) :: restar_en_catalogo xs pedido
		| (id2, cant2) :: ys -> if id = id2 then
			let a = ((id, cant - cant2), precio) in
			a :: restar_en_catalogo xs  ys	(* asumo ambas listas ordenadas, sino no funca ni a palos *)
			else
				((id, cant2), precio) :: restar_en_catalogo xs pedido
(* auxiliar de solicitar *)
let rec solicitar catalogo carrito =
	match carrito with
	| [] -> 0
	| (id, cant) :: xs -> let (_,c) = find2 catalogo id in
							cant*c + solicitar catalogo xs

(* auxiliar de avandonar*)
let rec anular_carrito catalogo carrito =
	match catalogo with
	| [] -> []
	| ((id, cant), _) :: xs ->
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
	| ((id, cant), precio) :: xs -> let (a,_) = producto in
							if a = id then
								let d = diferencia carrito producto in
								((id, cant + d),precio) :: devolver_a_catalogo xs carrito producto
							else
								((id, cant),precio) :: devolver_a_catalogo xs carrito producto
(* auxiliares de finalizar 
let rec cobrar catalogo carrito =
	match carrito with
    | [] -> true
    | (id, cant) :: xs -> let ((_,b),_) = find2 catalogo id in
    						if b  >= cant then
    						cobrar catalogo xs else false
*)



(* FUNCIONES PRINCIPALES *)
let rec servicio s cat carr =
 	match S.branch s with
  	(* Recibimos catalogo, carrito y pedido. Verificamos que las cantidades del pedido puedan satisfacerse, agregamos esas cantidades al carrito y
  	las quitamos del catalogo. Sino, devolvemos la lista de los elementos del pedido con las cant del catalogo *)
  	| `Pedir s -> let p, s = S.receive s in
	       let r = verif_pedido cat p in
	       let s = S.send r s in
	       if r = false then
		       let t = devolver_disponibles cat p in 	
		       let s = S.send t s in 
		       servicio s cat carr
		   else
		   		let t = restar_en_catalogo cat p in
		   		let q = sumar_en_carrito carr p in
		        let s = S.send q s in 	(* Seria mejor un select, xq en este lado del if no necesito mandar nada *)
		       	servicio s t q

	(* Solicitar multiplica cant * precio para cada elemento del carrito *)
	| `Solicitar s -> let r = (solicitar cat carr) in
	       print_list print_int carr;
	       print_newline();
	       print_int r;
	       print_newline();
	       servicio s cat carr

	(*la idea es agarrar la lista del carrito y volcarla en el catalogo  *)
  	| `Abandonar s -> S.close s

	(* quita el elemento pedido del carrito. Si se quieren quitar mas o los mismos elementos que hay, se quita el item *)
	| `Quitar s -> let q, s = S.receive s in
			let cat2 = devolver_a_catalogo cat carr q in
			let carr2 = quitar_carrito carr q in
		    servicio s cat2 carr2
(*
	| `Finalizar1 s ->  let r = Random.bool () in
	       	if r = true then
	      	    	let s = S.select (fun x -> `Salir x) s in
	      	    	S.close s
		    else
			let s = S.select (fun x -> `Fallo x) s in
			servicio s cat carr
*)
	| `Finalizar2 s -> let r, s = S.receive s in 	(* Para poder testear todo el punto 2 al mismo tiempo, cambio el random x un recieve *)
	       	if r = true then
	      	    	let s = S.select (fun x -> `Salir x) s in
	      	    	S.close s
		    else
			let s = S.select (fun x -> `Fallo x) s in
			servicio s cat carr

let server s =
	let cat = (crear_catalogo 10) in
	let carr = [] in
	servicio s cat carr


(* aca simulamos el cliente, y hacemos q llame al servidor con los distintos datos "hardcodeados". Como si el cliente lo recibiera de la persona. *)
let client s =
	print_string "b)";
	print_newline();
	let s = S.select (fun x -> `Finalizar2 x) s in 	(* Primero intento un pago *)
	let s = S.send false s in 						(* Le digo que falle *)
   	match S.branch s with
	    `Fallo s -> 
		    print_string " El pago fallo como esperaba";
		    print_newline();
			let s = S.select (fun x -> `Pedir x) s in 		(* ahora hago un pedido luego de intentar pagar *)
			let pedido = [(1,1)] in 						(* este pedido siempre esta *)
			let s = S.send pedido s in
			let ok, s = S.receive s in
			let _, s = S.receive s in
			Printf.printf " Pude realizar el pedido luego de que falle: %B" ok;
		    print_newline();

			print_string "d)";
			print_newline();
			let s = S.select (fun x -> `Solicitar x) s in 	(* Verifico que la lista esta vacia *)
			let s = S.select (fun x -> `Quitar x) s in 		(* Quito un producto que se que no esta. No pasa nada *)
			let quitar = (5,7) in
			let s = S.send quitar s in
			print_string " Pude quitar el elemento 5 del carrito que no estaba (no afecta al carrito ni al catalogo)";
		    print_newline();

			print_string "c)";
			print_newline();
			let s = S.select (fun x -> `Quitar x) s in 		(* Dejo la lista vacia *)
			let quitar = (1,1) in
			let s = S.send quitar s in
			print_string " Dejo la lista vacia:";
		    print_newline();
			let s = S.select (fun x -> `Solicitar x) s in 	(* Verifico que la lista esta vacia *)
			let s = S.select (fun x -> `Finalizar2 x) s in 	(* Finalizo sin problemas *)
			let s = S.send true s in 						(* En un sistema real, como le cobra $0 no hay problema (o se agregaria un if) *)
		   	(match S.branch s with
			    `Fallo s ->
					let s = S.select (fun x-> `Abandonar x) s in
					S.close s
				| `Salir s -> print_string "El pago se realizo correctamente";
					print_newline();
					S.close s)

		| `Salir s -> print_string " error ";
			print_newline();
			S.close s

 let _ =
  	let a, b = S.create () in
	let _ = Thread.create server a in
 	client b
 