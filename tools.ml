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
			(* first line is for the id, second is for the encrypted password
				and it repeats
			*)
			let id = input_line ic in
			let pw = input_line ic in
			(* construct the list with user credentials *)
			aux ((id, pw)::acc) (c+1)
		with End_of_file -> close_in ic;
		(* preserve file order *)
		(List.rev acc, c)
	in
	aux [] 0

let rec sanitize_data
	(data : (string * string) list)
	(res : (string * string) list)
	(i : int)
	: (string * string) list =
	let rec mem set x =
		if set = [] then false
		(* return true if x is equal to the first element of set,
			else go to next first element by reducing set
		*)
		else hd set = x || mem (tl set) x
	in
	let rec aux acc lst i =
		if lst = [] then begin
			printf "%d duplicated entries removed\n" i;
			(* preserve data order *)
			List.rev acc
		end else if mem (tl lst) (hd lst) then
			(* forget this entry and go to the next data entry *)
			sanitize_data acc (tl lst) (i+1)
		else
			(* reconstruct the list with scanned entry and
				go to the next data entry
			*)
			sanitize_data ((hd lst)::acc) (tl lst) i
	in
	aux res data i

let concatenate_same_datafiles (df : string list) : (string * string) list =
	let rec aux acc lst i =
		if lst = [] then
			if i > 1 then sanitize_data [] acc 0
			(* preserve data order *)
			else List.rev acc
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
