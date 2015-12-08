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

let ligne = 10;;
let colonne = 10;;

let remplir_matrice = fun file_path matrice ->
  let file = fic_ouvre_toi file_path in
  for i = 0 to ligne - 1 do
    for j = 0 to colonne - 1 do
      matrice.(i).(j) <- input_char file;
      Printf.printf "%c"matrice.(i).(j)
    done;
    Printf.printf"\n"
  done;;

let tab_words = fun matrice ->
  let liste_mots = ref [] in
  let x_val = ref 0 in
  let cpt = ref 0 in
  let var = ref {sens="horizontal"; ligne_colonne=0; debut=0; longueur=0} in
  for i = 0 to ligne - 1 do (*Horizontalement*)
      x_val := 0;
    for j = 0 to colonne - 1 do
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

  for j = 0 to colonne - 1 do (*Verticalement*)
    x_val := 0;
    for i = 0 to ligne - 1 do 
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
        liste_mots := xs; gen_tab () in 
  gen_tab ();;



let () =
  let matrix = (fun lig col init -> Array.init lig (fun _ -> Array.make col init)) ligne colonne '_' in
  remplir_matrice "fichier.txt" matrix;
  let table_mots = tab_words matrix in ();
  Printf.printf "Succes\n";;
