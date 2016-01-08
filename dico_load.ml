(* Charge le dictionnaire en fonction des mots rencontrés *)

type nlist =
    {mutable taille : int;
      mutable liste : string list};;

exception Empty

let empty = {taille=0; liste = []};;

let create = fun l -> {taille = List.length l; liste = l}

let add_nlist = fun chaine nliste ->
   {taille = nliste.taille + 1; liste = chaine :: nliste.liste};;

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

let gen_tableau = fun taille_max -> (* Génère un tableau de nlistes *)
  let tableau = (Array.init (taille_max +1) (fun i -> empty)) in
  tableau;;


let read_file = fun channel tableau taille_min -> (* Charge le tableau de nlistes avec les mots du dictionnaire *)
  let rec encore = fun () ->
      begin
        let l = input_line channel in
        let length = String.length l in
        let taille_max = Array.length tableau in
        if length <= (taille_max-1) && length >= taille_min then
          begin
            let new_domain = add_nlist l tableau.(length) in
            tableau.(length) <- new_domain;
          end;
        encore ()
      end in
  try encore () with End_of_file -> close_file channel;
    tableau;;


let dico_array = fun filename lmin lmax -> (* Exporte le tableau de nlistes contenant les mots chargés du dictionnaire *)
  let channel = open_file filename in
  let dico_table = gen_tableau lmax in
  read_file channel dico_table lmin;;




      
let print_tableau = fun tableau ->
  for i=0 to (Array.length tableau)-1 do
    Printf.printf "Nouvelle liste : mot de taille %d\n" i;
    Printf.printf "%d %d\n" (List.length (tableau.(i).liste)) tableau.(i).taille;
  done;;

let printf_nlist = fun nlist ->
  Printf.printf "nlist de taille %d\n" nlist.taille;
  List.iter (fun x -> Printf.printf "%s\n" x) nlist.liste;;
      
let main = fun () ->
  Printf.printf "Coucou\n";
  (* let channel = open_file "dico.txt" in *)
  Printf.printf "Channel open\n";
  (* let tableau1 = gen_tableau 10 in *)
  Printf.printf "Blabla\n";
  let tableau2 = dico_array "dico.txt" 2 10 in
  Printf.printf "Fini\n";
  print_tableau tableau2;;
 (* printf_nlist tableau2.(2);; *)
main ();;
