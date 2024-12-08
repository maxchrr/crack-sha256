(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)
#require "cryptokit"
#require "base64"

open Printf
open List

let parse_datafile (df : string) (rev : int) : string list =
	let rec aux acc i =
		if i > rev then List.rev acc
		else
			(* Formal grammar for datafile path *)
			let parsed_df =
				"data/"
				^ df
				^ (if i <= 9 then "0" ^ string_of_int i else string_of_int i)
				^ ".txt"
			in
			printf "Datafile %s revision %d parsed as %s\n" df i parsed_df;
			(* Continue recursively for each file revision *)
			aux (parsed_df::acc) (i+1)
	in
	(* Start the recursion index at 1 because we don't have 'app00.txt' *)
	aux [] 1

let read_datafile (df : string) : (string * string) list * int =
	let ic = open_in df in
	printf "Reading file %s...\n" df;
	let rec aux acc i =
		try
			(* Read username and password from the file. *)
			let id = input_line ic in
			let pw = input_line ic in
			(* Construct the list with user credentials pair *)
			aux ((id, pw)::acc) (i+1)
		with End_of_file -> close_in ic;
		(* Preserve file order *)
		(List.rev acc, i)
	in
	aux [] 0

let sanitize_data
	(data : (string * string) list)
	(res : (string * string) list)
	: (string * string) list =
	(* Helper function to check if a value exists in a list *)
	let rec mem set x =
		if set = [] then false
		(* Recursively check if x is in the list *)
		else hd set = x || mem (tl set) x
	in
	let rec aux acc lst i =
		if lst = [] then begin
			printf "%d duplicated entries removed\n" i;
			(* preserve data order *)
			List.rev acc
		end else if mem (tl lst) (hd lst) then
			(* If the current entry is a duplicate, skip it *)
			aux acc (tl lst) (i+1)
		else
			(* Otherwise, add it to the accumulator and continue. *)
			aux ((hd lst)::acc) (tl lst) i
	in
	aux res data 0

let concatenate_same_datafiles (df : string list) : (string * string) list =
	let rec aux acc lst i =
		if lst = [] then
			(* If more than one file was processed,
				sanitize the accumulated data
			*)
			if i > 1 then sanitize_data acc []
			(* If only one file, no need to sanitize. *)
			else List.rev acc
		else begin
			(* Read the next data file *)
			let (data,nb_entries) = read_datafile (hd lst) in
			printf "Loaded %d password hash\n" nb_entries;
			(* Append the data from this file to the accumulator
				and continue
			*)
			aux (data@acc) (tl lst) (i+1)
		end
	in
	aux [] df 0

let read_wordlist (wl : string) : string list =
	let ic = open_in wl in
	let rec aux acc =
		try
			(* Read each word from the file *)
			let word = input_line ic in
			(* Construct the list with word *)
			aux (word::acc)
		with End_of_file -> close_in ic;
		List.rev acc
	in
	aux []

let hash_password (pw : string) : string =
	(* Encrypt password with SHA-256 and Base64 encoding (rfc4648) *)
	Base64.encode_exn (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) pw)

let encrypt_wordlist (wl : string list) : string list =
	let rec aux acc lst =
		if lst = [] then List.rev acc
		else
			(* Hash the current word and add it to the accumulator *)
			aux (hash_password (hd lst)::acc) (tl lst)
	in
	aux [] wl
