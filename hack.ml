(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#use "tools.ml"

open Printf
open List

(* type for user' credentials *)
type t = { app : string; id : string; pw : string }

(*let find_credentials_with_username login apps =
	let res = ref "" in
	let rec find_data acc =
		if acc = [] then ()
		else begin
			let (username,password) = hd acc in
			if username = login then begin
				printf "username %s password %s\n" username password;
				if password = !res then
					printf "USERNAME %s SAME PASSWORD %s\n" username password;
				res := password
			end;
			find_data (tl acc)
		end
	in
	let rec aux acc =
		if acc = [] then ()
		else begin
			let (app,rev) = hd acc in
			printf "analyze leak %s with %d revision\n" app rev;
			find_data (get_data_leak app rev);
			aux (tl acc)
		end
	in
	aux apps
*)

let match_by_id
	(data : (string * string) list)
	(res : (string * string) list)
	(id : string)
: unit =
	()

let match_by_password
	(data : (string * string) list)
	(res : (string * string) list)
	(pwc : string)
	(pwe : string)
: (string * string) list =
	let rec aux acc lst =
		if lst = [] then acc
		else begin
			let (id,pw) = hd lst in
			(* if password match, add entry to accumlated list *)
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
			(* operation on wl and encrypted_wl at same time *)
			(tl lst1)
			(tl lst2)
	in
	aux [] wl encrypted_wl

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
