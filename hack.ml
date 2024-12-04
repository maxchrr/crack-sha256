(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#use "tools.ml"

open Printf
open List

(* type for user' credentials *)
type t = { app : string; id: string; pw: string }

let concatenate_files f =
	let rec aux acc fl c =
		if fl = [] then begin
			printf "total leaked entries=%d\n" c;
			acc
		end else begin
			(* read each file entry in the list *)
			let data,e = read_datafile (hd fl) in
			printf "reading file %s with %d entries\n" (hd fl) e;
			(* append new readed data to the accumulated list *)
			aux (data@acc) (tl fl) (c+e)
		end
	in
	aux [] f 0

let sanitize_data l =
	let rec mem x acc =
		if acc = [] then false
		else hd acc = x || mem x (tl acc)
	in
	let rec aux acc c =
		if acc = [] then begin
			printf "total entries removed=%d\n" c;
			acc
		end else begin
			(* return if head element is already present in the tail list *)
			if mem (hd acc) (tl acc) then begin
				(*printf "remove %s entry\n" (fst hd);*)
				aux (tl acc) (c+1)
			end else
				(* go recursively to the end of the list *)
				(hd acc)::(aux (tl acc) c)
		end
	in
	aux l 0

let get_data_leak df rev =
	let files = parse_datafile df rev in
	let data = concatenate_files files in
	let sd = sanitize_data data in
	(*printf "data size for %s=%d\n"
		app
		(List.length data);
	printf "sanitized data size for %s=%d\n"
		app
		(List.length sd);*)
	sd

let find_credentials_with_username login apps =
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

let get_credentials_with_password pw apps =
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

let crack_with_wordlist (wl : string list) (df : string) : t list =
	let encrypted_wl = encrypt_wordlist wl in
	let (data,nb_entries) = read_datafile df in
	printf "Loaded %d password hash\n" nb_entries;
	let rec match_user_credential lst acc pwe pwc =
		if lst = [] then acc
		else begin
			let (username,password) = hd lst in
			if password = pwe then begin
				printf "app: %s\nid: %s\npw: %s\n\n"
					df
					username
					pwc;
				match_user_credential
					(tl lst)
					({ app = df; id = username; pw = pwc }::acc)
					pwe
					pwc
			end else
				match_user_credential (tl lst) acc pwe pwc
		end
	in
	let rec test_pw lst1 lst2 acc =
		if lst1 = [] then acc
		else test_pw
			(tl lst1)
			(tl lst2)
			(match_user_credential data acc (hd lst1) (hd lst2))
	in
	test_pw encrypted_wl wl []
