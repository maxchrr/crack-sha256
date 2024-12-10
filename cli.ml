(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

(* skibidi -w=french_passwords_top20000.txt leaks *)

(* crack password hashes *)
(* skibidi [path/to/hashes.txt] *)

(* show passwords cracked *)
(* skibidi --show [path/to/hashes.txt] *)
(* skibidi -s [path/to/hashes.txt] *)

(* display users' cracked passwords by user identifier from multiple files *)
(* skibidi --s --users=[user_ids] [path/to/hashes1.txt path/to/hashes2.txt ... *)
(* skibidi -s -u=[user_ids] [path/to/hashes1.txt path/to/hashes2.txt ... *)

(* crack password hashes from directory recursively *)
(* skibidi -r [path/to/hashes/] *)

(* crack password hashes, using a custom wordlist *)
(* skikidi --wordlist=[path/to/wordlist.txt] [path/to/hashes.txt] *)
(* skibidi -w=[path/to/wordlist.txt] [path/to/hashes.txt] *)

open Tools

let usage_msg =
	"skibidi-cracker [-verbose] [-w <wordlist>] <file1> [<file2>] ..."

let verbose = ref false
let input_files = ref []
let wordlist = ref ""
let output_file = ref ""

let anon_fun filename =
	input_files := filename :: !input_files

let speclist =
	[("-verbose", Arg.Set verbose, "Output debug information");
	("-w", Arg.Set_string wordlist, "Crack with given wordlist")]

let () =
	Arg.parse speclist anon_fun usage_msg;
	let files = parse_input_files !input_files in
	let rec aux lst =
		if lst = [] then ()
		else begin
			let (app,rev) = List.hd lst in
			Printf.printf "file %s rev %d\n" app rev;
			aux (List.tl lst)
		end
	in
	aux files
