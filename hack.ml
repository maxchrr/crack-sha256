(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

open Printf;;

let parse_files n r =
	let rec aux acc i =
		if i > r then
			acc
		else
			(* formal grammar for file path *)
			let parsed_name =
				"leaks/"
				^ n
				^ (if i <= 9 then "0" ^ string_of_int i else string_of_int i)
				^ ".txt"
			in
			printf "leak %s revision %d parsed as %s\n" n i parsed_name;
			(* construct the list with each file revision *)
			aux (parsed_name :: acc) (i+1)
	in
	aux [] 1
;;
