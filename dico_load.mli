val file : string
type nlist = { mutable taille : int; mutable liste : string list; }
exception Empty
val empty : nlist
val add_nlist : string -> nlist -> nlist
val take_nlist : nlist -> string * nlist
val open_file : string -> in_channel
val close_file : in_channel -> unit
val gen_tableau : int -> nlist array
val read_file : in_channel -> nlist array -> int -> unit
val print_tableau : nlist array -> unit
val printf_nlist : nlist -> unit
val main : unit -> unit
