type word = {
  vertical : bool;
  ligne_colonne : int;
  debut : int;
  longueur : int;
  }
val copy_word : word -> word
type variable = {
  id : int;
  mutable domain : Dico_load.nlist;
  word : word;
  mutable crossed : int list;
  mutable instance : bool;
  }
val copy_var : variable -> variable
val get_grid : string -> string array
val get_vars : string array -> Dico_load.nlist array -> variable array
val print_tab_var : out_channel -> variable array -> unit
val min_length : int ref
val max_length : int ref
