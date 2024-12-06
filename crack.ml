(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)
#use "tools.ml"

open Printf
open List

(* type for user' credentials *)
type t = { app : string; id : string; pw : string }

let find_by_login
	(data: (string * string) list)
	(res : (string * string) list)
	(clear_id : string)
	(clear_pw : string)
	: (string * string) list =
	let rec aux acc lst =
		if lst = [] then begin
			printf "%d CREDENTIALS FOUND\n" (length acc);
			acc
		end else begin
			let (id,pw) = hd lst in
			(* if clear credentials are the same as in user credentials,
				add them to acc list
			*)
			if id = clear_id && pw = hash_password clear_pw then begin
				printf "------------------------------\n";
				printf "USER CREDENTIALS FOUND!\n";
				printf "id: %s\nhash pw: %s\n" id pw;
				printf "clear pw: %s\n" clear_pw;
				printf "------------------------------\n";
				aux ((id,clear_pw)::acc) (tl lst)
			(* else go to next entry *)
			end else aux acc (tl lst)
		end
	in
	aux res data

let crack_with_clear_data
	(clear_data : (string * string) list)
	(data : (string * string) list)
	: (string * string) list =
	let rec aux acc lst =
		if lst = [] then acc
		else begin
			let (id,pw) = hd lst in
			printf "id: %s\npw: %s\n" id pw;
			aux
				(* pass over each data entry to check whether it satisfies
					the predicates
				*)
				(find_by_login data acc id pw)
				(* got to next entry *)
				(tl lst)
		end
	in
	aux [] clear_data

let find_by_password
	(data : (string * string) list)
	(res : (string * string) list)
	(clear_pw : string)
	(encrypted_pw : string)
	: (string * string) list =
	let rec aux acc lst =
		if lst = [] then begin
			printf "%d CREDENTIAL FOUND\n" (length acc);
			acc
		end else begin
			let (id,pw) = hd lst in
			printf "------------------------------\n";
			printf "USER CREDENTIALS FOUND!\n";
			printf "id: %s\nhash pw: %s\n" id pw;
			printf "clear pw: %s\n" clear_pw;
			printf "------------------------------\n";
			(* if encrypted password is the same as in user credentials,
				add them with clear password to acc list
			*)
			if pw = encrypted_pw then aux ((id,clear_pw)::acc) (tl lst)
			(* else go to next entry *)
			else aux acc (tl lst)
		end
	in
	aux res data

let crack_with_wordlist
	(wl : string list)
	(data : (string * string) list)
	: (string * string) list =
	let encrypted_wl = encrypt_wordlist wl in
	let rec aux acc lst1 lst2 =
		if lst1 = [] && lst2 = [] then acc
		else begin
			let word = hd lst1 in
			let encrypted_word = hd lst2 in
			printf "word: %s\nencrypted word: %s\n" word encrypted_word;
			aux
				(* pass over each data entry to check whether it satisfies
					the predicates
				*)
				(find_by_password data acc word encrypted_word)
				(* go to next word (both clear and encrypted) *)
				(tl lst1)
				(tl lst2)
		end
	in
	aux [] wl encrypted_wl

let formalize_result
	(app : string)
	(data : (string * string) list)
: t list =
	let rec aux acc lst =
		if lst = [] then acc
		else
			let (id,pw) = hd lst in
			(* formalize decrypted data into the formal grammar of
				the record type
			*)
			aux ({ app=app; id=id; pw=pw }::acc) (tl lst)
	in
	aux [] data
