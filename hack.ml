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

(*let get_credentials_with_password pw apps =
	let rec find_data acc login =
		if acc = [] then ()
		else begin
			let (username,password) = hd acc in
			if password = pw then begin
				let (username,password) = hd acc in
				printf "password match with username %s\n" username;
				find_data (tl acc) (username::login)
			end;
			find_data (tl acc) login
		end
	in
	let rec aux acc =
		if acc = [] then ()
		else begin
			let (app,rev) = hd acc in
			printf "analyze leak %s with %d revision\n" app rev;
			find_data (get_data_leak app rev) [];
			aux (tl acc)
		end
	in
	aux apps
*)

let match_clean_password data =
	let rec aux acc res =
		if acc = [] then
			res
		else begin
			let (username,password) = hd acc in
			let hash_pw = hash_password password in
			printf "username: %s\npassword: %s\nhash password: %s\n\n"
				username
				password
				hash_pw;
			aux (tl acc) (hash_pw::res)
		end
	in
	aux data []

let crack_with_wordlist
	(wl : string list)
	(data : (string * string) list)
: (string * string) list =
	let encrypted_wl = encrypt_wordlist wl in
	let rec match_user_credential acc lst pwe pwc =
		if lst = [] then acc
		else begin
			let (username,password) = hd lst in
			(* password match *)
			if password = pwe then begin
				(*printf "id: %s\npw: %s\n\n"
					username
					pwc;*)
				match_user_credential
					(* add matched entry *)
					((username,pwc)::acc)
					(tl lst)
					pwe
					pwc
			end else
				match_user_credential acc (tl lst) pwe pwc
		end
	in
	let rec test_pw acc lst1 lst2 =
		if lst1 = [] then acc
		else test_pw
			(* pass on each data entry to test current password *)
			(match_user_credential data acc (hd lst1) (hd lst2))
			(* operation on wl and encrypted_wl at same time *)
			(tl lst1)
			(tl lst2)
	in
	test_pw [] encrypted_wl wl

(*let formalize_result (app : string) (res : (string * string) list) : t list =
	let rec aux lst acc =
		if lst = [] then acc
		else
			let (id,pw) = hd acc in
			aux (tl lst) ({ app = app ; id = id ; pw = pw }::acc)
	in
	aux res []
*)
