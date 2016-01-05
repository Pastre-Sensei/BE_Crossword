type word = {
  vertical : bool;
  ligne_colonne : int;
  debut : int;
  longueur : int;
}
type variable = {
  id : int;
  mutable domain : Dico_load.nlist;
  word : word;
  mutable crossed : int list;
  mutable instance : bool;
}
val fic_ouvre_toi : string -> in_channel
val close_file : in_channel -> unit
val read_grid : string -> string array
val gen_tab_words : string array -> word array
val var_table : word array -> variable array
val print_tab_words : word array -> unit
val print_crossed : int list -> unit
val print_var : variable -> unit
val print_tab_var : variable array -> unit
val main : string -> variable array
