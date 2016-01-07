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
val get_grid : string -> string array
val get_vars : string array -> Dico_load.nlist array -> variable array
