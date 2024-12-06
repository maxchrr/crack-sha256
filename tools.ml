(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)
#require "cryptokit"
#require "base64"

open Printf
open List

let parse_datafile (df : string) (rev : int) : string list =
	let rec aux acc i =
		if i > rev then List.rev acc
		else
			(* formal grammar for datafile path *)
			let parsed_df =
				"data/"
				^ df
				^ (if i <= 9 then "0" ^ string_of_int i else string_of_int i)
				^ ".txt"
			in
			printf "Datafile %s revision %d parsed as %s\n" df i parsed_df;
			(* construct the list with each file revision *)
			aux (parsed_df::acc) (i+1)
	in
	(* start index at 1 because we don't have 'app00.txt' *)
	aux [] 1

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

let sanitize_data
	(data : (string * string) list)
	(res : (string * string) list)
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
			aux acc (tl lst) (i+1)
		else
			(* reconstruct the list with scanned entry and
				go to the next data entry
			*)
			aux ((hd lst)::acc) (tl lst) i
	in
	aux res data 0

let concatenate_same_datafiles (df : string list) : (string * string) list =
	let rec aux acc lst i =
		if lst = [] then
			if i > 1 then sanitize_data acc []
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
