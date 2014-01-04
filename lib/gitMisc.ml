(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* From OCaml's stdlib. See [Digest.to_hex] *)
let hex_encode s =
  let n = String.length s in
  let result = String.create (n*2) in
  for i = 0 to n-1 do
    String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result

(* From OCaml's stdlib. See [Digest.from_hex] *)
let hex_decode h =
  let n = String.length h in
  if n mod 2 <> 0 then (
    let msg =
      Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
    raise (Invalid_argument msg)
  );
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | c ->
      let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
      raise (Invalid_argument msg) in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = String.create (n / 2) in
  for i = 0 to n/2 - 1 do
    result.[i] <- Char.chr (byte (2 * i));
  done;
  result

(* From OPAM's [OpamMisc.cut_at] *)
let cut_at_aux fn s sep =
  try
    let i = fn s sep in
    let name = String.sub s 0 i in
    let version = String.sub s (i+1) (String.length s - i - 1) in
    Some (name, version)
  with _ ->
    None

let cut_at = cut_at_aux String.index

let rcut_at = cut_at_aux String.rindex

let split s c =
  Re_pcre.split ~rex:(Re_perl.compile (Re.char c)) s

(* From Zlib *)
module Zlib_ext = struct

  let buffer_size = 1024
  let uncompress ?(header = true) incr_used_in refill flush =
    let inbuf = String.create buffer_size
    and outbuf = String.create buffer_size in
    let zs = Zlib.inflate_init header in
    let rec uncompr inpos inavail =
      if inavail = 0 then begin
        let incount = refill inbuf in
        if incount = 0 then uncompr_finish true else uncompr 0 incount
      end else begin
        let (finished, used_in, used_out) =
          Zlib.inflate zs inbuf inpos inavail outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
        incr_used_in used_in;
        flush outbuf used_out;
        if not finished then uncompr (inpos + used_in) (inavail - used_in)
      end
    and uncompr_finish first_finish =
      (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
         after the compressed stream in order to complete decompression
         and return finished = true. *)
      let dummy_byte = if first_finish && not header then 1 else 0 in
      let (finished, used_in, used_out) =
        Zlib.inflate zs inbuf 0 dummy_byte outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
      incr_used_in used_in;
      flush outbuf used_out;
      if not finished then uncompr_finish false
    in
    uncompr 0 0;
    Zlib.inflate_end zs

end

let uncompress_with_size ?header refill flush =
  let used_in = ref 0 in
  let incr_used_in n =
    used_in := !used_in + n in
  Zlib_ext.uncompress ?header incr_used_in refill flush;
  !used_in

let refill_string input =
  let n = String.length input in
  let toread = ref n in
  fun buf ->
    let m =
      if !toread <= String.length buf then !toread
      else String.length buf in
    String.blit input (n - !toread) buf 0 m;
    toread := !toread - m;
    m

let flush_string output buf len =
  Buffer.add_substring output buf 0 len

let deflate_string input =
  let output = Buffer.create 1024 in
  Zlib.compress (refill_string input) (flush_string output);
  Buffer.contents output

let deflate_mstruct buf =
  let inflated = Mstruct.get_string buf (Mstruct.length buf) in
  let deflated = deflate_string inflated in
  Mstruct.of_string deflated

let inflate_mstruct ?allocator orig_buf =
  let buf = Mstruct.clone orig_buf in
  let output = Buffer.create 1024 in
  let refill input =
    let n = min (Mstruct.length buf) (String.length input) in
    let s = Mstruct.get_string buf n in
    (* XXX: we could directly blit the bigarray into the string *)
    String.blit s 0 input 0 n;
    n in
  let flush buf len =
    Buffer.add_substring output buf 0 len in
  let size = uncompress_with_size refill flush in
  let inflated = Buffer.contents output in
  let res = Mstruct.of_string ?allocator inflated in
  Mstruct.shift orig_buf size;
  res

let mstruct_of_file file =
  let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o644 in
  let len =
    let stats = Unix.stat file in
    stats.Unix.st_size in
  let ba = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false len in
  Mstruct.of_bigarray ba

open Core_kernel.Std

let mkdir dirname =
  let rec aux dir =
    if Sys.file_exists dir then ()
    else (
      aux (Filename.dirname dir);
      Unix.mkdir dir 0o755
    ) in
  aux dirname

let list_files kind dir =
  if Sys.file_exists dir then
    let d = Sys.readdir dir in
    let d = Array.to_list d in
    let d = List.rev_map ~f:(Filename.concat dir) d in
    let d = List.filter ~f:kind d in
    List.sort compare d
  else
    []

let directories dir =
  list_files (fun f -> try Sys.is_directory f with _ -> false) dir

let files dir =
  list_files (fun f -> try not (Sys.is_directory f) with _ -> false) dir

let rec_files dir =
  let rec aux accu dir =
    let d = directories dir in
    let f = files dir in
    List.fold_left ~f:aux ~init:(f @ accu) d in
  aux [] dir

module OP = struct

  let (/) = Filename.concat

end