(* Charge le dictionnaire en fonction des mots rencontrés *)

let file = "dico.txt";;

type nlist =
    {taille : int;
      liste : string list};;
exception Empty
let empty = {taille=0; liste = []};;

let add_nlist = fun chaine nliste ->
  {taille = liste.taille + 1; liste = chaine :: nliste};;

let take_nlist = fun  nliste ->
  match nliste.liste with
    [] -> raise Empty
  | elt::reste -> (elt, {taille= nliste.taille -1; liste = reste});;

let open_file = fun file ->
  try
    open_in file
  with exc ->
    Printf.printf "%s ne s ouvre pas en lecture\n" file;
    raise exc;;

let gen_tableau = fun taille_max ->
  Array.make taille_max [];;


let read_file = fun channel tableau ->
  let rec encore = fun () ->
    let l = input_line channel in
    try
      let length = String.length l in
      tableau(

    with End_of_file ->
      close_in channel;
      failwith "Fin du document";;


