(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#require "cryptokit"
#require "base64"

open Printf
open List

let read_datafile (df : string) : (string * string) list * int =
	let ic = open_in df in
	printf "Reading file %s...\n" df;
	let rec aux acc c =
		try
			(* first line is for the username, second is for the password *)
			(* and it repeats *)
			let username = input_line ic in
			let password = input_line ic in
			(* construct the list with credential *)
			aux ((username, password)::acc) (c+1)
		with End_of_file -> close_in ic;
		(* preserve file order *)
		(List.rev acc, c)
	in
	aux [] 0

let rec mem (acc : 'a list) (x : 'a) : bool =
	if acc = [] then false
	else hd acc = x || mem (tl acc) x

let concatenate_same_datafiles (df : string list) : (string * string) list =
	let rec sanitize_data acc lst i =
		if lst = [] then begin
			printf "%d duplicated entries removed\n" i;
			List.rev acc
		end else if mem (tl lst) (hd lst) then
			sanitize_data acc (tl lst) (i+1)
		else
			(* go recursively to the end of the list *)
			sanitize_data ((hd lst)::acc) (tl lst) i
	in
	let rec aux acc lst i =
		if lst = [] then
			if i > 1 then begin
				sanitize_data [] acc 0
			end else List.rev acc
		else begin
			(* read each file entry in the list *)
			let (data,nb_entries) = read_datafile (hd lst) in
			printf "Loaded %d password hash\n" nb_entries;
			(* append new readed data to the accumulated list *)
			aux (data@acc) (tl lst) (i+1)
		end
	in
	aux [] df 0

let read_wordlist (wl : string) : string list =
	let ic = open_in wl in
	let rec aux acc =
		try
			(* each line contains one and only one word *)
			let word = input_line ic in
			(* construct the list with readed word *)
			aux (word::acc)
		with End_of_file -> close_in ic;
		List.rev acc
	in
	aux []

let hash_password (pw : string) : string =
	(* encrypt password with sha256 and base64 encoding (rfc4648) *)
	Base64.encode_exn (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) pw)

let encrypt_wordlist (wl : string list) : string list =
	let rec aux acc lst =
		if lst = [] then List.rev acc
		(* construct the list with password hashes *)
		else aux (hash_password (hd lst)::acc) (tl lst)
	in
	aux [] wl
