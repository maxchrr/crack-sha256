(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)
(*#use "tools.ml"*)

open Printf
open List
open Tools

(* Type for storing user credentials *)
type t = { app : string; id : string; pw : string }

let partition_by_login
	(acc : (string * string) list * (string * string) list)
	(data : (string * string) list)
	(clear_id : string)
	(clear_pw : string)
	: (string * string) list * (string * string) list =
	let rec part yes no lst =
		if lst = [] then
			(* The order of the elements in data is preserved *)
			(rev yes, rev no)
		else
			let (id,pw) = hd lst in
			(*printf "id: %s, hash pw: %s\n" id pw;*)
			(* Predicate over id and password *)
			if id = clear_id && pw = hash_password clear_pw then begin
				(*printf "FIND clear pw %s\n" clear_pw;*)
				(* Add to the first list 'yes' that satisfy the predicate
					and continue with the rest of data
				*)
				part ((id,clear_pw)::yes) no (tl lst)
			end else
				(* Otherwise, add to the second list 'no' (all the elements
					that do not satisfy) and continue
				*)
				part yes ((id,pw)::no) (tl lst)
	in
	(* Call the recursive partition function with splited accumulator *)
	part (fst acc) (snd acc) data

let crack_with_clear_data
	(clear_data : (string * string) list)
	(data : (string * string) list)
	: (string * string) list * (string * string) list =
	let rec aux acc lst rest =
		if lst = [] then acc
		else begin
			let (id,pw) = hd lst in
			let (yes,no) =
				(* Partition based on matching clear data credentials *)
				partition_by_login (fst acc,[]) rest id pw
			in
			(* Continue recursively with the rest of clear data
				and non-matching data
			*)
			aux (yes,no) (tl lst) no
		end
	in
	aux ([],[]) clear_data data

let partition_by_password
	(acc : (string * string) list * (string * string) list)
	(data : (string * string) list)
	(clear_pw : string)
	(encrypted_pw : string)
	: (string * string) list * (string * string) list =
	let rec part yes no lst =
		if lst = [] then (rev yes, rev no)
		else
			let (id,pw) = hd lst in
			(*printf "id: %s, hash pw: %s\n" id pw;*)
			(* Predicate over password *)
			if pw = encrypted_pw then begin
				(*printf "FIND clear pw %s\n" clear_pw;*)
				(* Add to the first list 'yes' that satisfy the predicate
					and continue with the rest of data
				*)
				part ((id,clear_pw)::yes) no (tl lst)
			end else
				(* Otherwise, add to the second list 'no' (all the elements
					that do not satisfy) and continue
				*)
				part yes ((id,pw)::no) (tl lst)
	in
	(* Call the recursive partition function with splited accumulator *)
	part (fst acc) (snd acc) data

let crack_with_wordlist
	(wl : string list)
	(data : (string * string) list)
	: (string * string) list * (string * string) list =
	let encrypted_wl = encrypt_wordlist wl in
	let rec aux acc lst1 lst2 rest =
		if lst1 = [] && lst2 = [] then acc
		else begin
			let word = hd lst1 in
			let encrypted_word = hd lst2 in
			(*printf "\nword: %s, encrypted word: %s\n" word encrypted_word;*)
			let (yes,no) =
				(* Partition the data by comparing the encrypted word. *)
				partition_by_password (fst acc,[]) rest word encrypted_word
			in
			(* Continue recursively with the rest of the wordlist
				and non-matching data
			*)
			aux (yes,no) (tl lst1) (tl lst2) no
		end
	in
	aux ([],[]) wl encrypted_wl data

let formalize_result
	(app : string)
	(data : (string * string) list)
	: t list =
	let rec aux acc lst =
		if lst = [] then acc
		else
			let (id,pw) = hd lst in
			(* Add a formatted credential entry to the result *)
			aux ({ app=app; id=id; pw=pw }::acc) (tl lst)
	in
	(* Start the recursion with the matched credentials *)
	aux [] data
