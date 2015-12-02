(* Charge le dictionnaire en fonction des mots rencontrés *)

let file = "dico.txt";;

type nlist =
    {mutable taille : int;
      mutable liste : string list};;

exception Empty

let empty = {taille=0; liste = []};;

let add_nlist = fun chaine nliste ->
  (* {taille = nliste.taille + 1; liste = chaine :: nliste.liste};; *)
  begin
    nliste.taille <- nliste.taille +1;
    nliste.liste <- chaine::nliste.liste;
  end

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

let close_file = fun channel ->
  try 
    close_in channel
  with exc ->
    Printf.printf "%s ne se ferme pas bien\n" "dico.txt";
    raise exc;;

let gen_tableau = fun taille_max ->
  let tableau = ref (Array.init (taille_max+1) (fun _ -> empty)) in
  tableau;;


let read_file = fun channel tableau taille_max ->
  let compteur = ref 0 in
  let rec encore = fun () ->
      begin
        let l = input_line channel in
        let length = String.length l in
        if length <= taille_max then begin
        (* tableau.(length) <- add_nlist l tableau.(length); *)
          add_nlist l !tableau.(length);
          Printf.printf "%s %d\n" l length;
          if length = 4 then begin incr compteur; Printf.printf "%d\n" !compteur end;
        end;
        encore ()
      end in
    try encore () with End_of_file -> close_file channel;;


let main = fun () ->
  let channel = open_file file in
  let tableau1 = gen_tableau 8 in
  read_file channel tableau1 8;
  Printf.printf "%d\n" !tableau1.(4).taille;
  Printf.printf "Fini\n";;

main ();;
  
