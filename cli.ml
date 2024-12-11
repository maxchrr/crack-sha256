(* Copyright (c) 2024 Max Charrier, Inès Schneider. All Rights Reserved. *)

open Crack
open Tools

(* usage message printed in the case of a malformed command line,
	or when help is requested
*)
let usage_msg : string =
	"skibidi-cracker [-verbose] [-w <wordlist>] <file1> [<file2>] ..."

(* references to hold the information gathered from the command line *)
let verbose = ref false
let clear_files = ref []
let input_files = ref []
let wordlist = ref ""
let output_file = ref ""

(* function to handle the anonymous inputs, simply add the file to the ref *)
let anon_fun (filename : string) : unit =
	clear_files := filename::(!clear_files)

(* function to handle rest option, which is file *)
let rest_fun (file : string) : unit =
	input_files := file::(!input_files)

(* speclist is a list of triples (key, spec, doc) *)
let speclist : (Arg.key * Arg.spec * Arg.doc) list =
	[
		("-verbose", Arg.Set verbose, "Output debug information");
		("-w", Arg.Set_string wordlist, "Crack with given wordlist");
		("--", Arg.Rest rest_fun, "Files to crack")
	]

let crack_data
	(clear_data : (string * string) list)
	(wl : string list)
	(files : string list list)
	: Crack.t list =
	let rec aux acc lst1 =
		if lst1 = [] then acc
		else begin
			let files = List.hd lst1 in
			let parsed_data = fst (List.hd (parse_files files)) in
			let data = concatenate_same_datafiles files in
			let res1 = crack_with_clear_data clear_data data in
			let res2 = crack_with_wordlist wl (snd res1) in
			let data_decrypted =
				formalize_result parsed_data ((fst res1)@(fst res2))
			in
			aux (data_decrypted@acc) (List.tl lst1)
		end
	in
	aux [] files

let () =
	Arg.parse speclist anon_fun usage_msg;
	if !input_files = [] then begin
		Printf.printf "Please provide files to crack\n";
		exit 1
	end;
	if !wordlist = "" then begin
		Printf.printf "Please provide wordlist with -w option\n";
		exit 1
	end;
	(* Preserve written order *)
	clear_files := List.rev !clear_files;
	input_files := List.rev !input_files;
	let parsed_files = parse_files !input_files in
	let splited_files = split_same_files !input_files parsed_files in
	let clear_data = concatenate_same_datafiles !clear_files in
	let wl = read_wordlist !wordlist in
	let res = crack_data clear_data wl splited_files in
	Printf.printf
		"%d password decrypted, see output for more details\n"
		(List.length res)
