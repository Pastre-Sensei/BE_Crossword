type word = {
  mutable sens : string;
  mutable ligne_colonne : int;
  mutable debut : int;
  mutable longueur : int;
}
val fic_ouvre_toi : string -> in_channel
val ligne : int
val colonne : int
val min_word : int ref
val max_word : int ref
val remplir_matrice : string -> char array array -> unit
val tab_words : char array array -> word array
val minmax_word : unit -> int * int
