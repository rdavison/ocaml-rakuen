#!/usr/bin/env ocaml

#use "topfind"
#require "camlimages.png"
#require "parmap"

open Images
open OImages
open Parmap

let cpu_count = (* credit: http://stackoverflow.com/a/16273514 *)
  try match Sys.os_type with 
  | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS") 
  | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ 
  | End_of_file | Unix.Unix_error (_, _, _) -> 1

let split_ext s =
  let len = String.length s in
  let rec find_dot i =
    if i < 0 then None
    else if s.[i] = '.' then Some i
    else find_dot (i-1)
  in
  let oindex = find_dot (len-1) in
  match oindex with
  | Some i -> (String.sub s 0 i, Some (String.sub s (i + 1) (len - i - 1)))
  | None -> (s, None)

let tag msg fname =
  let head, ext = split_ext fname in
  let marked = head ^ "-" ^ msg in
  match ext with
  | Some ext -> marked ^ "." ^ ext
  | None -> marked

let crop fname ~top ~left ~right ~bottom =
  let tagged = tag "cropped" fname in
  let img = load fname [] in
  let width, height = size img#image in
  let cropped = sub img left top (width - left + right) (height - top + bottom) in
  cropped#save tagged None []

let endswith s subs =
  let len_s = String.length s in
  let len_subs = String.length subs in
  let rec loop i j =
    if i = len_s && j = len_subs then
      true
    else if i < len_s && j < len_subs && s.[i] = subs.[j] then
        loop (i+1) (j+1)
    else
      false
  in
  loop (len_s - len_subs) 0

let () =
  (* config *)
  let dir = ".." in
  let top = 194 in
  let left = 0 in
  let right = -1592 in
  let bottom = -20 in
  let ext = ".png" in
  
  (* Define application *)
  let files = Array.map (fun x -> dir ^ "/" ^ x) (Sys.readdir dir) in
  let seq = A files in
  let app fname =
    if endswith fname ext then begin
      crop fname ~top ~left ~right ~bottom;
      print_endline ("Cropped " ^ fname);
    end
  in

  (* Parallelize the application *)
  Parmap.pariter app seq ~ncores:cpu_count
