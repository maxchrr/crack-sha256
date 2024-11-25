(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

let read_data_file f =
	let ic = open_in f in
	let rec aux acc c =
		try
			(* first line is for the username, second is for the password *)
			(* and it repeats *)
			let username = input_line ic in
			let password = input_line ic in
			(* construct the list with credential *)
			aux ((username, password) :: acc) (c+1)
		with End_of_file -> close_in ic;
		(* preserve file order *)
		(List.rev acc, c)
	in
	aux [] 0

(*#use "topfind";;
#require "cryptokit";;
#require "base64";;

let hash_password pwd =
	Base64.encode_exn(Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) pwd)
*)
