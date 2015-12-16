type word = {
  sens : bool;
  ligne_colonne : int;
  debut : int;
  longueur : int;
}
val fic_ouvre_toi : string -> in_channel
val close_file : in_channel -> unit
val read_grid : string -> string array
val gen_tab_words : string array -> word array
val print_tab_words : word array -> unit
val main : string -> word array
