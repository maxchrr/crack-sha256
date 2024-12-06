(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#use "tools.ml"

open Printf
open List

(* type for user' credentials *)
type t = { app : string; id : string; pw : string }

let match_by_password
	(data : (string * string) list)
	(res : (string * string) list)
	(pwc : string)
	(pwe : string)
: (string * string) list =
	let rec aux acc lst =
		if lst = [] then begin
			printf "\n%d CREDENTIAL MATCH\n" (length acc);
			acc
		end else begin
			let (id,pw) = hd lst in
			(* if encrypted password is the same as in user credential,
				add them with clear password to acc list
			*)
			if pw = pwe then aux ((id,pwc)::acc) (tl lst)
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
		else aux
			(* pass on each data entry to test current password *)
			(match_by_password data acc (hd lst1) (hd lst2))
			(* operations on wl and encrypted_wl at same time *)
			(tl lst1)
			(tl lst2)
	in
	aux [] wl encrypted_wl

let crack_with_clear_data
	(clear_data : (string * string) list)
	(data : (string * string) list)
: (string * string) list =
	let rec find_in_data acc lst cred =
		if lst = [] then acc
		else
			let (id,pw) = hd lst in
			if id = fst cred && hash_password pw = snd cred then begin
				printf "------------------------------\n";
				printf "CREDENTIAL MATCH\n";
				printf "id: %s\nclear pw: %s\n" id pw;
				printf "hash pw: %s\n" (snd cred);
				printf "------------------------------\n";
				find_in_data ((id,pw)::acc) (tl lst) cred
			end else find_in_data acc (tl lst) cred
	in
	let rec aux acc lst =
		if lst = [] then begin
			printf "\n%d CREDENTIAL MATCH\n" (length acc);
			acc
		end else begin
			let (id,pw) = hd lst in
			printf "id: %s\npw: %s\n" id pw;
			aux (find_in_data acc data (id,pw)) (tl lst)
		end
	in
	aux [] clear_data

(*let formalize_result
	(app : string)
	(res : (string * string) list)
: t list =
	let rec aux lst acc =
		if lst = [] then acc
		else
			let (id,pw) = hd acc in
			aux (tl lst) ({ app = app ; id = id ; pw = pw }::acc)
	in
	aux res []
*)
