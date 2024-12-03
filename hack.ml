(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

#use "tools.ml"

open Printf
open List

(* type for user' credentials *)
type t = { app : string; id: string; pw: string }

let parse_datafile (df : string) (rev : int) : string list =
	let rec aux acc i =
		if i > rev then acc
		else
			(* formal grammar for datafile path *)
			let parsed_df =
				"leaks/"
				^ n
				^ (if i <= 9 then "0" ^ string_of_int i else string_of_int i)
				^ ".txt"
			in
			printf "Datafile %s revision %d parsed as %s\n" n i parsed_df;
			(* construct the list with each file revision *)
			aux (parsed_df::acc) (i+1)
	in
	(* start index at 1 because we don't have 'app00.txt' *)
	aux [] 1

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

(* for loop *)
let rec match_password wl pw =
	if wl = [] then ""
	else if hash_password (hd wl) = pw then hd wl
	else match_password (tl wl) pw

(* while loop *)
let rec find_login wl data c =
	let cref = ref c in
	if data = [] then printf "%d password cracked\n" c
	else begin
		let (username,password) = hd data in
		let pw = match_password wl password in
		if pw <> "" then begin
			printf "username: %s\npassword: %s\n\n"
				username
				pw;
			cref := !cref+1
		end;
		find_login wl (tl data) !cref
	end

let get_credentials pw apps =
	let total = ref 0 in
	let rec aux acc =
		if acc = [] then begin
			printf "%d total password cracked\n\n" !total;
			()
		end
		else begin
			let (app,rev) = hd acc in
			printf "start cracking on %s\n" app;
			(*find_login pw (get_data_leak app rev) 0;*)
			let pwa = Array.of_list pw in
			let len = Array.length pwa in
			let data = get_data_leak app rev in
			let c = ref 0 in
			for i=0 to len-1 do
				let hashed_pw = hash_password pwa.(i) in
				let data = ref data in
				while !data <> [] do
					let (username,password) = hd !data in
					if password = hashed_pw then begin
						printf "app_name: %s\nusername: %s\npassword: %s\n\n"
							(app ^ " " ^ (string_of_int rev))
							username
							pwa.(i);
						c := !c+1
					end;
					data := tl !data
				done
			done;
			printf "%d password cracked on %s\n\n" !c app;
			total := !total + !c;
			aux (tl acc)
		end
	in
	aux apps
