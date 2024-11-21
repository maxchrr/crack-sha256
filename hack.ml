(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#use "tools.ml";;

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

let concatenate_files f =
	let rec aux acc fl c =
		match fl with
		| [] ->
			printf "total leaked entries=%d\n" c;
			acc
		| hd::tl ->
			(* read each file entry in the list *)
			let data,e = read_data_file hd in
			printf "reading file %s with %d entries\n" hd e;
			(* append new readed data to the accumulated list *)
			aux (data @ acc) tl (c+e)
	in
	aux [] f 0
;;

let sanitize_data l =
	let rec aux acc c =
		match acc with
		| [] ->
			printf "total entries removed=%d\n" c;
			acc
		| hd::tl ->
			(* return if head element is already present in the tail list *)
			if List.mem hd tl then begin
				(*printf "remove %s entry\n" (fst hd);*)
				aux tl (c+1)
			end else
				(* go recursively to the end of the list *)
				hd :: (aux tl c)
	in
	aux l 0
;;

let get_data_leak app rev =
	let files = parse_files app rev in
	let data = concatenate_files files in
	let sd = sanitize_data data in
	printf "data size for %s=%d\n"
		app
		(List.length data);
	printf "sanitized data size for %s=%d\n"
		app
		(List.length sd)
;;
