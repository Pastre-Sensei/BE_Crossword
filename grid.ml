type word = {
   mutable sens : string;
   mutable ligne_colonne : int;
   mutable debut : int;
   mutable longueur : int

};;

let fic_ouvre_toi = fun file_path ->
  try
    open_in file_path
  with exc ->
    Printf.printf "%s ne s ouvre pas en lecture\n" file_path;
    raise exc;;

let ligne = ref 1;;
let colonne = ref 0;;
let min_word  = ref max_int;;
let max_word  = ref 0;;

let remplir_matrice = fun file_path ->
  let flag = ref 0 in
  let var = ref ' ' in
  let liste = ref [] in
  let file = fic_ouvre_toi file_path in
  let rec encore = fun () ->
    var := input_char file;
    if (!var) != '\n' then
      begin
        liste:=(!var) :: (!liste);
        if !flag = 0 then
          incr colonne;
      end
    else
      begin
        incr ligne;
        flag:=1;
      end;
    let matrix = Array.make_matrix !ligne !colonne '_' in
    for i = 0 to !ligne - 1  do
      for j = 0 to !colonne - 1  do
        begin
          match !liste with
            [] -> ();
          | x::xs ->
              matrix.(i).(j) <- x;
              liste:=xs;
        end
      done
    done
  in try encore () with End_of_file -> () ;;

let tab_words = fun matrice ->
  let liste_mots = ref [] in
  let x_val = ref 0 in
  let cpt = ref 0 in
  let var = ref {sens="horizontal"; ligne_colonne=0; debut=0; longueur=0} in
  for i = 0 to !ligne - 1 do (*Horizontalement*)
      x_val := 0;
    for j = 0 to !colonne - 1 do
      if matrice.(i).(j)='*' then 
        begin
          if (!cpt) >= 2 then
            begin
              var := {sens="horizontal"; ligne_colonne=i; debut=(j-(!cpt)); longueur=(!cpt)};
              liste_mots := !var :: !liste_mots;
            end;
          cpt:=0;
        end
      else
        cpt:=!cpt+1;
      x_val := !x_val + 1;
    done;
    if (!cpt) != 0 then
      begin
        if (!cpt) >= 2 then
          begin
            var := {sens="horizontal"; ligne_colonne=i; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
            liste_mots := !var :: !liste_mots;
          end;
        cpt:=0;
      end
  done;

  for j = 0 to !colonne - 1 do (*Verticalement*)
    x_val := 0;
    for i = 0 to !ligne - 1 do 
      if matrice.(i).(j)='*' then
          begin
            if (!cpt) >= 2 then
              begin
                var := {sens="vertical"; ligne_colonne=j; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
                liste_mots := !var :: !liste_mots;
              end;
            cpt:=0; 
        end
      else
        cpt:=!cpt+1;
      x_val := !x_val + 1;
    done;
    if (!cpt) != 0 then
      begin
        if (!cpt) >= 2 then
          begin
            var := {sens="Vertical"; ligne_colonne=j; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
            liste_mots := !var :: !liste_mots;
          end;
        cpt:=0;
      end
  done;
  let words_table = Array.make (List.length !liste_mots) {sens=""; ligne_colonne=0; debut=0; longueur=0} in
  let rec gen_tab = fun () ->
    match !liste_mots with
      [] -> words_table;
    | x::xs -> 
        words_table.((List.length !liste_mots) - 1) <- x; 
        if (!min_word) > x.longueur then
          min_word:=x.longueur;
        if (!max_word) < x.longueur then
          max_word:=x.longueur;
        liste_mots := xs; gen_tab () in 
  gen_tab ();;

let minmax_word = fun () ->
  (!min_word, !max_word);;

(*
let () =
  let matrix = remplir_matrice "fichier.txt" in matrix;
  let table_mots = tab_words matrix in ();
  Printf.printf "Succes\n";;
*)
