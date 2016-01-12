type nlist = { mutable taille : int; mutable liste : string list; }
exception Empty
val fprint_domain : out_channel -> nlist -> unit
val empty : nlist
val create : string list -> nlist
val add_nlist : string -> nlist -> nlist
val take_nlist : nlist -> string * nlist

val dico_array : string -> int -> int -> nlist array (* La fonction utile *)

val print_tableau : nlist array -> unit
val printf_nlist : nlist -> unit
